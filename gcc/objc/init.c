/* GNU Objective C Runtime initialization 
   Copyright (C) 1993, 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup
   +load support contributed by Ovidiu Predescu <ovidiu@net-community.com>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
GNU CC; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include "runtime.h"

/* The version number of this runtime.  This must match the number 
   defined in gcc (objc-act.c) */
#define OBJC_VERSION 8
#define PROTOCOL_VERSION 2

/* This list contains all modules currently loaded into the runtime */
static struct objc_list* __objc_module_list = 0; 	/* !T:MUTEX */

/* This list contains all proto_list's not yet assigned class links */
static struct objc_list* unclaimed_proto_list = 0; 	/* !T:MUTEX */

/* List of unresolved static instances.  */
static struct objc_list *uninitialized_statics = 0; 	/* !T:MUTEX */

/* Global runtime "write" mutex. */
objc_mutex_t __objc_runtime_mutex = 0;

/* Number of threads that are alive. */
int __objc_runtime_threads_alive = 1;			/* !T:MUTEX */

/* Check compiler vs runtime version */
static void init_check_module_version (Module_t);

/* Assign isa links to protos */
static void __objc_init_protocols (struct objc_protocol_list* protos);

/* Add protocol to class */
static void __objc_class_add_protocols (Class, struct objc_protocol_list*);

/* This is a hook which is called by __objc_exec_class every time a class
   or a category is loaded into the runtime.  This may e.g. help a
   dynamic loader determine the classes that have been loaded when
   an object file is dynamically linked in */
void (*_objc_load_callback)(Class class, Category* category); /* !T:SAFE */

/* Is all categories/classes resolved? */
BOOL __objc_dangling_categories = NO;           /* !T:UNUSED */

extern SEL
__sel_register_typed_name (const char *name, const char *types, 
			   struct objc_selector *orig, BOOL is_const);

/* Sends +load to all classes and categories in certain situations. */
static void objc_send_load (void);

/* Inserts all the classes defined in module in a tree of classes that
   resembles the class hierarchy. This tree is traversed in preorder and the
   classes in its nodes receive the +load message if these methods were not
   executed before. The algorithm ensures that when the +load method of a class
   is executed all the superclasses have been already received the +load
   message. */
static void __objc_create_classes_tree (Module_t module);

static void __objc_call_callback (Module_t module);

/* A special version that works only before the classes are completely
   installed in the runtime. */
static BOOL class_is_subclass_of_class (Class class, Class superclass);

typedef struct objc_class_tree {
  Class class;
  struct objc_list *subclasses; /* `head' is pointer to an objc_class_tree */
} objc_class_tree;

/* This is a linked list of objc_class_tree trees. The head of these trees
   are root classes (their super class is Nil). These different trees
   represent different class hierarchies. */
static struct objc_list *__objc_class_tree_list = NULL;

/* Keeps the +load methods who have been already executed. This hash should
   not be destroyed during the execution of the program. */
static cache_ptr __objc_load_methods = NULL;

/* Creates a tree of classes whose topmost class is directly inherited from
   `upper' and the bottom class in this tree is `bottom_class'. The classes
   in this tree are super classes of `bottom_class'. `subclasses' member
   of each tree node point to the next subclass tree node. */
static objc_class_tree *
create_tree_of_subclasses_inherited_from (Class bottom_class, Class upper)
{
  Class superclass = bottom_class->super_class ?
			objc_lookup_class ((char*)bottom_class->super_class)
		      : Nil;
					
  objc_class_tree *tree, *prev;

  DEBUG_PRINTF ("create_tree_of_subclasses_inherited_from:");
  DEBUG_PRINTF ("bottom_class = %s, upper = %s\n",
		(bottom_class ? bottom_class->name : NULL),
		(upper ? upper->name : NULL));

  tree = prev = objc_calloc (1, sizeof (objc_class_tree));
  prev->class = bottom_class;

  while (superclass != upper)
    {
      tree = objc_calloc (1, sizeof (objc_class_tree));
      tree->class = superclass;
      tree->subclasses = list_cons (prev, tree->subclasses);
      superclass = (superclass->super_class ?
			objc_lookup_class ((char*)superclass->super_class)
		      : Nil);
      prev = tree;
    }

  return tree;
}

/* Insert the `class' into the proper place in the `tree' class hierarchy. This
   function returns a new tree if the class has been successfully inserted into
   the tree or NULL if the class is not part of the classes hierarchy described
   by `tree'. This function is private to objc_tree_insert_class(), you should
   not call it directly. */
static objc_class_tree *
__objc_tree_insert_class (objc_class_tree *tree, Class class)
{
  DEBUG_PRINTF ("__objc_tree_insert_class: tree = %x, class = %s\n",
		tree, class->name);

  if (tree == NULL)
    return create_tree_of_subclasses_inherited_from (class, NULL);
  else if (class == tree->class)
    {
      /* `class' has been already inserted */
      DEBUG_PRINTF ("1. class %s was previously inserted\n", class->name);
      return tree;
    }
  else if ((class->super_class ?
		    objc_lookup_class ((char*)class->super_class)
		  : Nil)
	    == tree->class)
    {
      /* If class is a direct subclass of tree->class then add class to the
	 list of subclasses. First check to see if it wasn't already
	 inserted. */
      struct objc_list *list = tree->subclasses;
      objc_class_tree *node;

      while (list)
	{
	  /* Class has been already inserted; do nothing just return
	     the tree. */
	  if (((objc_class_tree*)list->head)->class == class)
	    {
	      DEBUG_PRINTF ("2. class %s was previously inserted\n",
			    class->name);
	      return tree;
	    }
	  list = list->tail;
	}

      /* Create a new node class and insert it into the list of subclasses */
      node = objc_calloc (1, sizeof (objc_class_tree));
      node->class = class;
      tree->subclasses = list_cons (node, tree->subclasses);
      DEBUG_PRINTF ("3. class %s inserted\n", class->name);
      return tree;
    }
  else
    {
      /* The class is not a direct subclass of tree->class. Search for class's
         superclasses in the list of subclasses. */
      struct objc_list *subclasses = tree->subclasses;

      /* Precondition: the class must be a subclass of tree->class; otherwise
         return NULL to indicate our caller that it must take the next tree. */
      if (!class_is_subclass_of_class (class, tree->class))
	return NULL;

      for (; subclasses != NULL; subclasses = subclasses->tail)
	{
	  Class aClass = ((objc_class_tree*)(subclasses->head))->class;

	  if (class_is_subclass_of_class (class, aClass))
	    {
	      /* If we found one of class's superclasses we insert the class
	         into its subtree and return the original tree since nothing
		 has been changed. */
	      subclasses->head
		  = __objc_tree_insert_class (subclasses->head, class);
 	      DEBUG_PRINTF ("4. class %s inserted\n", class->name);
	      return tree;
	    }
	}

      /* We haven't found a subclass of `class' in the `subclasses' list.
         Create a new tree of classes whose topmost class is a direct subclass
	 of tree->class. */
      {
	objc_class_tree *new_tree
	    = create_tree_of_subclasses_inherited_from (class, tree->class);
	tree->subclasses = list_cons (new_tree, tree->subclasses);
 	DEBUG_PRINTF ("5. class %s inserted\n", class->name);
	return tree;
      }
    }
}

/* This function inserts `class' in the right tree hierarchy classes. */
static void
objc_tree_insert_class (Class class)
{
  struct objc_list *list_node;
  objc_class_tree *tree;

  list_node = __objc_class_tree_list;
  while (list_node)
    {
      tree = __objc_tree_insert_class (list_node->head, class);
      if (tree)
	{
	  list_node->head = tree;
	  break;
	}
      else
	list_node = list_node->tail;
    }

  /* If the list was finished but the class hasn't been inserted, insert it
     here. */
  if (!list_node)
    {
      __objc_class_tree_list = list_cons (NULL, __objc_class_tree_list);
      __objc_class_tree_list->head = __objc_tree_insert_class (NULL, class);
    }
}

/* Traverse tree in preorder. Used to send +load. */
static void
objc_preorder_traverse (objc_class_tree *tree,
			int level,
			void (*function)(objc_class_tree*, int))
{
  struct objc_list *node;

  (*function) (tree, level);
  for (node = tree->subclasses; node; node = node->tail)
    objc_preorder_traverse (node->head, level + 1, function);
}

/* Traverse tree in postorder. Used to destroy a tree. */
static void
objc_postorder_traverse (objc_class_tree *tree,
			int level,
			void (*function)(objc_class_tree*, int))
{
  struct objc_list *node;

  for (node = tree->subclasses; node; node = node->tail)
    objc_postorder_traverse (node->head, level + 1, function);
  (*function) (tree, level);
}

/* Used to print a tree class hierarchy. */
static void
__objc_tree_print (objc_class_tree *tree, int level)
{
  int i;

  for (i = 0; i < level; i++)
    printf ("  ");
  printf ("%s\n", tree->class->name);
}

/* Walks on a linked list of methods in the reverse order and executes all
   the methods corresponding to `op' selector. Walking in the reverse order
   assures the +load of class is executed first and then +load of categories
   because of the way in which categories are added to the class methods. */
static void
__objc_send_message_in_list (MethodList_t method_list, Class class, SEL op)
{
  int i;

  if (!method_list)
    return;

  /* First execute the `op' message in the following method lists */
  __objc_send_message_in_list (method_list->method_next, class, op);

  /* Search the method list. */
  for (i = 0; i < method_list->method_count; i++)
    {
      Method_t mth = &method_list->method_list[i];

      if (mth->method_name && sel_eq (mth->method_name, op)
	  && !hash_is_key_in_hash (__objc_load_methods, mth->method_name))
	{
	  /* The method was found and wasn't previously executed. */
	  (*mth->method_imp) ((id)class, mth->method_name);

	  /* Add this method into the +load hash table */
	  hash_add (&__objc_load_methods, mth->method_imp, mth->method_imp);

	  DEBUG_PRINTF ("sending +load in class: %s\n", class->name);

	  break;
	}
    }
}

static void
__objc_send_load (objc_class_tree *tree, int level)
{
  static SEL load_sel = 0;
  Class class = tree->class;
  MethodList_t method_list = class->class_pointer->methods;

  if (!load_sel)
    load_sel = sel_register_name ("load");

  __objc_send_message_in_list (method_list, class, load_sel);
}

static void
__objc_destroy_class_tree_node (objc_class_tree *tree, int level)
{
  objc_free (tree);
}

/* This is used to check if the relationship between two classes before the
   runtime completely installs the classes. */
static BOOL
class_is_subclass_of_class (Class class, Class superclass)
{
  for (; class != Nil;)
    {
      if (class == superclass)
	return YES;
      class = (class->super_class ?
		  objc_lookup_class ((char*)class->super_class)
		: Nil);
    }

  return NO;
}

/* This list contains all the classes in the runtime system for whom their
   superclasses are not yet know to the runtime. */
static struct objc_list* unresolved_classes = 0;

/* Static function used to reference the Object and NXConstantString classes.
 */
static void
__objc_force_linking (void)
{
  extern void __objc_linking (void);
  __objc_linking ();

  /* Call the function to avoid compiler warning */
  __objc_force_linking ();
}

/* Run through the statics list, removing modules as soon as all its statics
   have been initialized.  */
static void
objc_init_statics (void)
{
  struct objc_list **cell = &uninitialized_statics;
  struct objc_static_instances **statics_in_module;

  objc_mutex_lock(__objc_runtime_mutex);

  while (*cell)
    {
      int module_initialized = 1;

      for (statics_in_module = (*cell)->head;
	   *statics_in_module; statics_in_module++)
	{
	  struct objc_static_instances *statics = *statics_in_module;
	  Class class = objc_lookup_class (statics->class_name);

	  if (!class)
	    module_initialized = 0;
	  /* Actually, the static's class_pointer will be NULL when we
             haven't been here before.  However, the comparison is to be
             reminded of taking into account class posing and to think about
             possible semantics...  */
	  else if (class != statics->instances[0]->class_pointer)
	    {
	      id *inst;

	      for (inst = &statics->instances[0]; *inst; inst++)
		{
		  (*inst)->class_pointer = class;

		  /* ??? Make sure the object will not be freed.  With
                     refcounting, invoke `-retain'.  Without refcounting, do
                     nothing and hope that `-free' will never be invoked.  */

		  /* ??? Send the object an `-initStatic' or something to
                     that effect now or later on?  What are the semantics of
                     statically allocated instances, besides the trivial
                     NXConstantString, anyway?  */
		}
	    }
	}
      if (module_initialized)
	{
	  /* Remove this module from the uninitialized list.  */
	  struct objc_list *this = *cell;
	  *cell = this->tail;
	  objc_free(this);
	}
      else
	cell = &(*cell)->tail;
    }

  objc_mutex_unlock(__objc_runtime_mutex);
} /* objc_init_statics */

/* This function is called by constructor functions generated for each
   module compiled.  (_GLOBAL_$I$...) The purpose of this function is to
   gather the module pointers so that they may be processed by the
   initialization routines as soon as possible */

void
__objc_exec_class (Module_t module)
{
  /* Have we processed any constructors previously?  This flag is used to
     indicate that some global data structures need to be built.  */
  static BOOL previous_constructors = 0;

  static struct objc_list* unclaimed_categories = 0;

  /* The symbol table (defined in objc-api.h) generated by gcc */
  Symtab_t symtab = module->symtab;

  /* The statics in this module */
  struct objc_static_instances **statics
    = symtab->defs[symtab->cls_def_cnt + symtab->cat_def_cnt];

  /* Entry used to traverse hash lists */
  struct objc_list** cell;

  /* The table of selector references for this module */
  SEL selectors = symtab->refs; 

  /* dummy counter */
  int i;

  DEBUG_PRINTF ("received module: %s\n", module->name);

  /* check gcc version */
  init_check_module_version(module);

  /* On the first call of this routine, initialize some data structures.  */
  if (!previous_constructors)
    {
	/* Initialize thread-safe system */
      __objc_init_thread_system();
      __objc_runtime_threads_alive = 1;
      __objc_runtime_mutex = objc_mutex_allocate();

      __objc_init_selector_tables();
      __objc_init_class_tables();
      __objc_init_dispatch_tables();
      __objc_class_tree_list = list_cons (NULL, __objc_class_tree_list);
      __objc_load_methods
	  = hash_new (128, (hash_func_type)hash_ptr, compare_ptrs);
      previous_constructors = 1;
    }

  /* Save the module pointer for later processing. (not currently used) */
  objc_mutex_lock(__objc_runtime_mutex);
  __objc_module_list = list_cons(module, __objc_module_list);

  /* Replace referenced selectors from names to SEL's.  */
  if (selectors)
    {
      for (i = 0; selectors[i].sel_id; ++i)
	{
	  const char *name, *type;
	  name = (char*)selectors[i].sel_id;
	  type = (char*)selectors[i].sel_types;
	  /* Constructors are constant static data so we can safely store
	     pointers to them in the runtime structures. is_const == YES */
	  __sel_register_typed_name (name, type, 
				     (struct objc_selector*)&(selectors[i]),
				     YES);
	}
    }

  /* Parse the classes in the load module and gather selector information.  */
  DEBUG_PRINTF ("gathering selectors from module: %s\n", module->name);
  for (i = 0; i < symtab->cls_def_cnt; ++i)
    {
      Class class = (Class) symtab->defs[i];
      const char* superclass = (char*)class->super_class;

      /* Make sure we have what we think.  */
      assert (CLS_ISCLASS(class));
      assert (CLS_ISMETA(class->class_pointer));
      DEBUG_PRINTF ("phase 1, processing class: %s\n", class->name);

      /* Initialize the subclass list to be NULL.
	 In some cases it isn't and this crashes the program. */
      class->subclass_list = NULL;

      /* Store the class in the class table and assign class numbers.  */
      __objc_add_class_to_hash (class);

      /* Register all of the selectors in the class and meta class.  */
      __objc_register_selectors_from_class (class);
      __objc_register_selectors_from_class ((Class) class->class_pointer);

      /* Install the fake dispatch tables */
      __objc_install_premature_dtable(class);
      __objc_install_premature_dtable(class->class_pointer);

      /* Register the instance methods as class methods, this is
	 only done for root classes. */
      __objc_register_instance_methods_to_class(class);

      if (class->protocols)
	__objc_init_protocols (class->protocols);

      /* Check to see if the superclass is known in this point. If it's not
	 add the class to the unresolved_classes list. */
      if (superclass && !objc_lookup_class (superclass))
	unresolved_classes = list_cons (class, unresolved_classes);
   }

  /* Process category information from the module.  */
  for (i = 0; i < symtab->cat_def_cnt; ++i)
    {
      Category_t category = symtab->defs[i + symtab->cls_def_cnt];
      Class class = objc_lookup_class (category->class_name);
      
      /* If the class for the category exists then append its methods.  */
      if (class)
	{

	  DEBUG_PRINTF ("processing categories from (module,object): %s, %s\n",
			module->name,
			class->name);

	  /* Do instance methods.  */
	  if (category->instance_methods)
	    class_add_method_list (class, category->instance_methods);

	  /* Do class methods.  */
	  if (category->class_methods)
	    class_add_method_list ((Class) class->class_pointer, 
				   category->class_methods);

	  if (category->protocols)
	    {
	      __objc_init_protocols (category->protocols);
	      __objc_class_add_protocols (class, category->protocols);
	    }

          /* Register the instance methods as class methods, this is
             only done for root classes. */
          __objc_register_instance_methods_to_class(class);
	}
      else
	{
	  /* The object to which the category methods belong can't be found.
	     Save the information.  */
	  unclaimed_categories = list_cons(category, unclaimed_categories);
	}
    }

  if (statics)
    uninitialized_statics = list_cons (statics, uninitialized_statics);
  if (uninitialized_statics)
    objc_init_statics ();

  /* Scan the unclaimed category hash.  Attempt to attach any unclaimed
     categories to objects.  */
  for (cell = &unclaimed_categories;
       *cell;
       ({ if (*cell) cell = &(*cell)->tail; }))
    {
      Category_t category = (*cell)->head;
      Class class = objc_lookup_class (category->class_name);
      
      if (class)
	{
	  DEBUG_PRINTF ("attaching stored categories to object: %s\n",
			class->name);
	  
	  list_remove_head (cell);
	  
	  if (category->instance_methods)
	    class_add_method_list (class, category->instance_methods);
	  
	  if (category->class_methods)
	    class_add_method_list ((Class) class->class_pointer,
				   category->class_methods);

	  if (category->protocols)
	    {
	      __objc_init_protocols (category->protocols);
	      __objc_class_add_protocols (class, category->protocols);
	    }

          /* Register the instance methods as class methods, this is
             only done for root classes. */
          __objc_register_instance_methods_to_class(class);
	}
    }
  
  if (unclaimed_proto_list && objc_lookup_class ("Protocol"))
    {
      list_mapcar (unclaimed_proto_list,(void(*)(void*))__objc_init_protocols);
      list_free (unclaimed_proto_list);
      unclaimed_proto_list = 0;
    }

  objc_send_load ();

  objc_mutex_unlock(__objc_runtime_mutex);
}

static void objc_send_load (void)
{
  if (!__objc_module_list)
    return;
 
  /* Try to find out if all the classes loaded so far also have their
     superclasses known to the runtime. We suppose that the objects that are
     allocated in the +load method are in general of a class declared in the
     same module. */
  if (unresolved_classes)
    {
      Class class = unresolved_classes->head;

      while (objc_lookup_class ((char*)class->super_class))
	{
	  list_remove_head (&unresolved_classes);
	  if (unresolved_classes)
	    class = unresolved_classes->head;
	  else
	    break;
	}

      /*
       * If we still have classes for whom we don't have yet their super
       * classes known to the runtime we don't send the +load messages.
       */
      if (unresolved_classes)
	return;
    }

  /* Special check to allow creating and sending messages to constant strings
     in +load methods. If these classes are not yet known, even if all the
     other classes are known, delay sending of +load. */
  if (!objc_lookup_class ("NXConstantString") ||
      !objc_lookup_class ("Object"))
    return;

  /* Iterate over all modules in the __objc_module_list and call on them the
     __objc_create_classes_tree function. This function creates a tree of
     classes that resembles the class hierarchy. */
  list_mapcar (__objc_module_list, (void(*)(void*))__objc_create_classes_tree);

  while (__objc_class_tree_list)
    {
#ifdef DEBUG
      objc_preorder_traverse (__objc_class_tree_list->head,
			      0, __objc_tree_print);
#endif
      objc_preorder_traverse (__objc_class_tree_list->head,
			      0, __objc_send_load);
      objc_postorder_traverse (__objc_class_tree_list->head,
			      0, __objc_destroy_class_tree_node);
      list_remove_head (&__objc_class_tree_list);
    }

  list_mapcar (__objc_module_list, (void(*)(void*))__objc_call_callback);
  list_free (__objc_module_list);
  __objc_module_list = NULL;
}

static void
__objc_create_classes_tree (Module_t module)
{
  /* The runtime mutex is locked in this point */

  Symtab_t symtab = module->symtab;
  int i;

  /* Iterate thru classes defined in this module and insert them in the classes
     tree hierarchy. */
  for (i = 0; i < symtab->cls_def_cnt; i++)
    {
      Class class = (Class) symtab->defs[i];

      objc_tree_insert_class (class);
    }
}

static void
__objc_call_callback (Module_t module)
{
  /* The runtime mutex is locked in this point */

  Symtab_t symtab = module->symtab;
  int i;

  /* Iterate thru classes defined in this module and call the callback for
     each one. */
  for (i = 0; i < symtab->cls_def_cnt; i++)
    {
      Class class = (Class) symtab->defs[i];

      /* Call the _objc_load_callback for this class. */
      if (_objc_load_callback)
	_objc_load_callback(class, 0);
    }

  /* Call the _objc_load_callback for categories. Don't register the instance
     methods as class methods for categories to root classes since they were
     already added in the class. */
  for (i = 0; i < symtab->cat_def_cnt; i++)
    {
      Category_t category = symtab->defs[i + symtab->cls_def_cnt];
      Class class = objc_lookup_class (category->class_name);
      
      if (_objc_load_callback)
	_objc_load_callback(class, category);
    }
}

/* Sanity check the version of gcc used to compile `module'*/
static void init_check_module_version(Module_t module)
{
  if ((module->version != OBJC_VERSION) || (module->size != sizeof (Module)))
    {
      int code;

      if(module->version > OBJC_VERSION)
	code = OBJC_ERR_OBJC_VERSION;
      else if (module->version < OBJC_VERSION)
	code = OBJC_ERR_GCC_VERSION;
      else
	code = OBJC_ERR_MODULE_SIZE;

      objc_error(nil, code, "Module %s version %d doesn't match runtime %d\n",
	       module->name, (int)module->version, OBJC_VERSION);
    }
}

static void
__objc_init_protocols (struct objc_protocol_list* protos)
{
  int i;
  static Class proto_class = 0;

  if (! protos)
    return;

  objc_mutex_lock(__objc_runtime_mutex);

  if (!proto_class)
    proto_class = objc_lookup_class("Protocol");

  if (!proto_class)
    {
      unclaimed_proto_list = list_cons (protos, unclaimed_proto_list);
      objc_mutex_unlock(__objc_runtime_mutex);
      return;
    }

#if 0
  assert (protos->next == 0);	/* only single ones allowed */
#endif

  for(i = 0; i < protos->count; i++)
    {
      struct objc_protocol* aProto = protos->list[i];
      if (((size_t)aProto->class_pointer) == PROTOCOL_VERSION)
	{
	  /* assign class pointer */
	  aProto->class_pointer = proto_class;

	  /* init super protocols */
	  __objc_init_protocols (aProto->protocol_list);
	}
      else if (protos->list[i]->class_pointer != proto_class)
	{
	  objc_error(nil, OBJC_ERR_PROTOCOL_VERSION,
		     "Version %d doesn't match runtime protocol version %d\n",
		     (int)((char*)protos->list[i]->class_pointer-(char*)0),
		     PROTOCOL_VERSION);
	}
    }

  objc_mutex_unlock(__objc_runtime_mutex);
}

static void __objc_class_add_protocols (Class class,
					struct objc_protocol_list* protos)
{
  /* Well... */
  if (! protos)
    return;

  /* Add it... */
  protos->next = class->protocols;
  class->protocols = protos;
}
