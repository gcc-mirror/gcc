/* GNU Objective C Runtime message lookup 
   Copyright (C) 1993 Free Software Foundation, Inc.

Author: Kresten Krab Thorup

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
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include "runtime.h"
#include "sarray.h"

/* The uninstalled dispatch table */
struct sarray* __objc_uninstalled_dtable = 0;

/* Send +initialize to class */
static void __objc_send_initialize(Class*);

static void __objc_install_dispatch_table_for_class (Class*);

/* Forward declare some functions */
static void __objc_init_install_dtable(id, SEL);
static id __objc_missing_method(id, SEL, ...);
static Method_t search_for_method_in_hierarchy (Class* class, SEL sel);
static Method_t search_for_method_in_list(MethodList_t list, SEL op);
id nil_method(id, SEL, ...);

id
nil_method(id receiver, SEL op, ...)
{
  return receiver;
}

/* Given a class and selector, return the selector's implementation.  */
__inline__ IMP
get_imp (Class* class, SEL sel)
{
  void* res = sarray_get (class->dtable, (size_t) sel);
  if(res == __objc_init_install_dtable)
    __objc_install_dispatch_table_for_class (class);
  return sarray_get (class->dtable, (size_t) sel);
}

__inline__ BOOL
__objc_responds_to (id object, SEL sel)
{
  return get_imp (object->class_pointer, sel) != __objc_missing_method;
}

/* This is the lookup function.  All entries in the table are either a 
   valid method *or* one of `__objc_missing_method' which calls
   forward:: etc, or `__objc_init_install_dtable' which installs the
   real dtable */
__inline__ IMP
objc_msg_lookup(id receiver, SEL op)
{
  if(receiver)
    return sarray_get(receiver->class_pointer->dtable, (sidx)op);
  else
    return nil_method;
}

IMP
objc_msg_lookup_super (Super_t super, SEL sel)
{
  if (super->self)
    return get_imp (super->class, sel);
  else
    return nil_method;
}

retval_t
objc_msg_sendv(id object, SEL op, size_t frame_size, arglist_t arg_frame)
{
#ifdef __objc_frame_receiver
  __objc_frame_receiver(arg_frame) = object;
  __objc_frame_selector(arg_frame) = op;
  return __builtin_apply((apply_t)get_imp(object->class_pointer, op),
			 arg_frame,
			 frame_size);
#else
#warning performv:: will not work
  va_list nothing;
  (*_objc_error)(object, "objc_msg_sendv (performv::) not supported\n", nothing);
  return 0;
#endif
}

void __objc_init_dispatch_tables()
{
  __objc_uninstalled_dtable
    = sarray_new(200, __objc_init_install_dtable);
}

/* This one is a bit hairy.  This function is installed in the 
   premature dispatch table, and thus called once for each class,
   namely when the very first message is send to it.  */

static void __objc_init_install_dtable(id receiver, SEL op)
{
  __label__ allready_initialized;
  IMP imp;
  void* args;
  void* result;

  /* This may happen, if the programmer has taken the address of a 
     method before the dtable was initialized... too bad for him! */
  if(receiver->class_pointer->dtable != __objc_uninstalled_dtable)
    goto allready_initialized;

  if(CLS_ISCLASS(receiver->class_pointer))
    {
      /* receiver is an ordinary object */
      assert(CLS_ISCLASS(receiver->class_pointer));

      /* install instance methods table */
      __objc_install_dispatch_table_for_class (receiver->class_pointer);

      /* call +initialize -- this will in turn install the factory 
	 dispatch table if not already done :-) */
      __objc_send_initialize(receiver->class_pointer);
    }
  else
    {
      /* receiver is a class object */
      assert(CLS_ISCLASS((Class*)receiver));
      assert(CLS_ISMETA(receiver->class_pointer));

      /* Install real dtable for factory methods */
      __objc_install_dispatch_table_for_class (receiver->class_pointer);
      
      if(op != sel_get_uid ("initialize"))
	__objc_send_initialize((Class*)receiver);
      else
	CLS_SETINITIALIZED((Class*)receiver);
    }

allready_initialized:
  
  /* Get real method for this in newly installed dtable */
  imp = get_imp(receiver->class_pointer, op);

  args = __builtin_apply_args();
  result = __builtin_apply((apply_t)imp, args, 96);
  __builtin_return (result);
  
}

/* Install dummy table for class which causes the first message to
   that class (or instances hereof) to be initialized properly */
void __objc_install_premature_dtable(Class* class)
{
  assert(__objc_uninstalled_dtable);
  class->dtable = __objc_uninstalled_dtable;
}   

/* Send +initialize to class if not already done */
static void __objc_send_initialize(Class* class)
{
  Method_t m;

  /* This *must* be a class object */
  assert(CLS_ISCLASS(class));
  assert(!CLS_ISMETA(class));

  if (!CLS_ISINITIALIZED(class))
    {
      CLS_SETINITIALIZED(class);
      CLS_SETINITIALIZED(class->class_pointer);
      
      if(class->super_class)
	__objc_send_initialize(class->super_class);

      {
	MethodList_t method_list = class->class_pointer->methods;
	SEL op = sel_register_name ("initialize");

	/* If not found then we'll search the list.  */
	while (method_list)
	  {
	    int i;

	    /* Search the method list.  */
	    for (i = 0; i < method_list->method_count; ++i)
	      {
		Method_t method = &method_list->method_list[i];
		
		
		if (method->method_name == op)
		  (*method->method_imp)((id) class, op);
	      }

	    /* The method wasn't found.  Follow the link to the next list of
	       methods.  */
	    method_list = method_list->method_next;
	  }
      }
    }
}  

static void
__objc_install_dispatch_table_for_class (Class* class)
{
  Class* super;
  MethodList_t mlist;
  int counter;

  /* If the class has not yet had it's class links resolved, we must 
     re-compute all class links */
  if(!CLS_ISRESOLV(class))
    __objc_resolve_class_links();

  super = class->super_class;

  if (super != 0 && (super->dtable == __objc_uninstalled_dtable))
    __objc_install_dispatch_table_for_class (super);

  /* Allocate dtable if nessecary */
  if (super == 0)
    {
      class->dtable = sarray_new (__objc_selector_max_index,
				  __objc_missing_method);
    }
  else
    class->dtable = sarray_lazy_copy (super->dtable);

  for (mlist = class->methods; mlist; mlist = mlist->method_next)
    {
      counter = mlist->method_count - 1;
      while (counter >= 0)
        {
          Method_t method = &(mlist->method_list[counter]);
	  sarray_at_put (class->dtable,
			 (sidx) method->method_name,
			 method->method_imp);
          counter -= 1;
        }
    }
}

void __objc_update_dispatch_table_for_class (Class* class)
{
  Class* next;
  struct sarray* save;

  /* not yet installed -- skip it */
  if (class->dtable == __objc_uninstalled_dtable) 
    return;

  save = class->dtable;
  __objc_install_premature_dtable (class);
  sarray_free (save);


  if (class->subclass_list)	/* Traverse subclasses */
    for (next = class->subclass_list; next; next = next->sibling_class)
      __objc_update_dispatch_table_for_class (next);
}


/* This function adds a method list to a class.  This function is
   typically called by another function specific to the run-time.  As
   such this function does not worry about thread safe issued.

   This one is only called for categories. Class objects have their
   methods installed rightaway, and their selectors are made into
   SEL's by the function __objc_register_selectors_from_class. */ 
void
class_add_method_list (Class* class, MethodList_t list)
{
  int i;
  static SEL initialize_sel = 0;
  if (!initialize_sel)
    initialize_sel = sel_register_name ("initialize");

  /* Passing of a linked list is not allowed.  Do multiple calls.  */
  assert (!list->method_next);

  /* Check for duplicates.  */
  for (i = 0; i < list->method_count; ++i)
    {
      Method_t method = &list->method_list[i];

      if (method->method_name)  /* Sometimes these are NULL */
	{
	  /* This is where selector names are transmogriffed to SEL's */
	  method->method_name = sel_register_name ((char*)method->method_name);

	  if (search_for_method_in_list (class->methods, method->method_name)
	      && method->method_name != initialize_sel)
	    {
	      /* Duplication. Print a error message an change the method name
		 to NULL. */
	      fprintf (stderr, "attempt to add a existing method: %s\n",
		       sel_get_name(method->method_name));
	      method->method_name = 0;
	    }
	}
    }

  /* Add the methods to the class's method list.  */
  list->method_next = class->methods;
  class->methods = list;
}


Method_t
class_get_instance_method(Class* class, SEL op)
{
  return search_for_method_in_hierarchy(class, op);
}

Method_t
class_get_class_method(MetaClass* class, SEL op)
{
  return search_for_method_in_hierarchy(class, op);
}


/* Search for a method starting from the current class up its hierarchy.
   Return a pointer to the method's method structure if found.  NULL
   otherwise. */   

static Method_t
search_for_method_in_hierarchy (Class* cls, SEL sel)
{
  Method_t method = NULL;
  Class* class;

  if (! sel_is_mapped (sel))
    return NULL;

  /* Scan the method list of the class.  If the method isn't found in the
     list then step to its super class. */
  for (class = cls; ((! method) && class); class = class->super_class)
    method = search_for_method_in_list (class->methods, sel);

  return method;
}



/* Given a linked list of method and a method's name.  Search for the named
   method's method structure.  Return a pointer to the method's method
   structure if found.  NULL otherwise. */  
static Method_t
search_for_method_in_list (MethodList_t list, SEL op)
{
  MethodList_t method_list = list;

  if (! sel_is_mapped (op))
    return NULL;

  /* If not found then we'll search the list.  */
  while (method_list)
    {
      int i;

      /* Search the method list.  */
      for (i = 0; i < method_list->method_count; ++i)
        {
          Method_t method = &method_list->method_list[i];

          if (method->method_name)
            if (method->method_name == op)
              return method;
        }

      /* The method wasn't found.  Follow the link to the next list of
         methods.  */
      method_list = method_list->method_next;
    }

  return NULL;
}


/* This fuction is installed in the dispatch table for all methods which are
   not implemented.  Thus, it is called when a selector is not recognized. */
static id
__objc_missing_method (id object, SEL sel, ...)
{
  IMP imp;
  SEL frwd_sel;
  SEL err_sel;

  /* first try if the object understands forward:: */
  frwd_sel = sel_get_uid("forward::");
  imp = get_imp(object->class_pointer, frwd_sel);
  if(imp != __objc_missing_method)
    {
      void *result, *args = __builtin_apply_args();
      result = (*imp)(object, frwd_sel, sel, args);
      __builtin_return(result);
    }

  /* If the object recognizes the doesNotRecognize: method then we're going
     to send it. */
  err_sel = sel_get_uid ("doesNotRecognize:");
  imp = get_imp (object->class_pointer, err_sel);
  if (imp != __objc_missing_method)
    {
      return (*imp) (object, err_sel, sel);
    }
  
  /* The object doesn't recognize the method.  Check for responding to
     error:.  If it does then sent it. */
  {
    char msg[256 + strlen ((char*)sel_get_name (sel))
             + strlen ((char*)object->class_pointer->name)];

    sprintf (msg, "(%s) %s does not recognize %s",
	     (CLS_ISMETA(object->class_pointer)
	      ? "class"
	      : "instance" ),
             object->class_pointer->name, sel_get_name (sel));

    err_sel = sel_get_uid ("error:");
    imp = get_imp (object->class_pointer, err_sel);
    if (imp != __objc_missing_method)
      return (*imp) (object, sel_get_uid ("error:"), msg);

    /* The object doesn't respond to doesNotRecognize: or error:;  Therefore,
       a default action is taken. */
    fprintf (stderr, "fatal: %s\n", msg);
    abort ();
  }
}

void __objc_print_dtable_stats()
{
  int total = 0;
  printf("memory usage: (%s)\n",
#ifdef OBJC_SPARSE2
	 "2-level sparse arrays"
#else
	 "3-level sparse arrays"
#endif
	 );

  printf("arrays: %d = %d bytes\n", narrays, narrays*sizeof(struct sarray));
  total += narrays*sizeof(struct sarray);
  printf("buckets: %d = %d bytes\n", nbuckets, nbuckets*sizeof(struct sbucket));
  total += nbuckets*sizeof(struct sbucket);

  printf("idxtables: %d = %d bytes\n", idxsize, idxsize*sizeof(void*));
  total += idxsize*sizeof(void*);
  printf("-----------------------------------\n");
  printf("total: %d bytes\n", total);
  printf("===================================\n");
}



