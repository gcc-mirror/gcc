/* GNU Objective C Runtime class related functions
   Copyright (C) 1993 Free Software Foundation, Inc.

Author: Kresten Krab Thorup, Dennis Glatting

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

#include "runtime.h"		/* the kitchen sink */
#include "sarray.h"

/* The table of classname->class.  Used for objc_lookup_class and friends */
static cache_ptr __objc_class_hash = 0;

/* This is a hook which is called by objc_get_class and 
   objc_lookup_class if the runtime is not able to find the class.
   This may e.g. try to load in the class using dynamic loading */
Class* (*_objc_lookup_class)(const char* name) = 0;


/* True when class links has been resolved */     
BOOL __objc_class_links_resolved = NO;


/* Initial number of buckets size of class hash table. */
#define CLASS_HASH_SIZE 32

void __objc_init_class_tables()
{
  /* Allocate the class hash table */

  if(__objc_class_hash)
    return;

  __objc_class_hash
    =  hash_new (CLASS_HASH_SIZE,
		 (hash_func_type) hash_string,
		 (compare_func_type) compare_strings);
}  

/* This function adds a class to the class hash table, and assigns the 
   class a number, unless it's already known */
void
__objc_add_class_to_hash(Class* class)
{
  Class* h_class;

  /* make sure the table is there */
  assert(__objc_class_hash);

  /* make sure it's not a meta class */  
  assert(CLS_ISCLASS(class));

  /* Check to see if the class is already in the hash table.  */
  h_class = hash_value_for_key (__objc_class_hash, class->name);
  if (!h_class)
    {
      /* The class isn't in the hash table.  Add the class and assign a class
         number.  */
      static unsigned int class_number = 1;

      CLS_SETNUMBER(class, class_number);
      CLS_SETNUMBER(class->class_pointer, class_number);

      ++class_number;
      hash_add (&__objc_class_hash, class->name, class);
    }
}

/* Get the class object for the class named NAME.  If NAME does not
   identify a known class, the hook _objc_lookup_class is called.  If
   this fails, nil is returned */
Class* objc_lookup_class (const char* name)
{
  Class* class;

  /* Make sure the class hash table exists.  */
  assert (__objc_class_hash);

  class = hash_value_for_key (__objc_class_hash, name);

  if (class)
    return class;

  if (_objc_lookup_class)
    return (*_objc_lookup_class)(name);
  else
    return 0;
}

/* Get the class object for the class named NAME.  If NAME does not
   identify a known class, the hook _objc_lookup_class is called.  If
   this fails,  an error message is issued and the system aborts */
Class*
objc_get_class (const char *name)
{
  Class* class;

  /* Make sure the class hash table exists.  */
  assert (__objc_class_hash);

  class = hash_value_for_key (__objc_class_hash, name);

  if (class)
    return class;

  if (_objc_lookup_class)
    class = (*_objc_lookup_class)(name);

  if(class)
    return class;
  
  fprintf(stderr, "objc runtime: cannot find class %s\n", name);
  abort();
}

MetaClass*
objc_get_meta_class(const char *name)
{
  return objc_get_class(name)->class_pointer;
}

/* This function provides a way to enumerate all the classes in the
   executable.  Pass *ENUM_STATE == NULL to start the enumeration.  The
   function will return 0 when there are no more classes.  
   For example: 
       id class; 
       void *es = NULL;
       while ((class = objc_next_class(&es)))
         ... do something with class; 
*/
Class* 
objc_next_class(void **enum_state)
{
  /* make sure the table is there */
  assert(__objc_class_hash);

  *(node_ptr*)enum_state = 
    hash_next(__objc_class_hash, *(node_ptr*)enum_state);
  if (*(node_ptr*)enum_state)
    return (*(node_ptr*)enum_state)->value;
  return (Class*)0;
}

/* Resolve super/subclass links for all classes.  The only thing we 
   can be sure of is that the class_pointer for class objects point 
   to the right meta class objects */
void __objc_resolve_class_links()
{
  node_ptr node;
  Class* object_class = objc_get_class ("Object");

  assert(object_class);

  /* Assign subclass links */
  for (node = hash_next (__objc_class_hash, NULL); node;
       node = hash_next (__objc_class_hash, node))
    {
      Class* class1 = node->value;

      /* Make sure we have what we think we have.  */
      assert (CLS_ISCLASS(class1));
      assert (CLS_ISMETA(class1->class_pointer));

      /* The class_pointer of all meta classes point to Object's meta class. */
      class1->class_pointer->class_pointer = object_class->class_pointer;

      if (!(CLS_ISRESOLV(class1)))
        {
          CLS_SETRESOLV(class1);
          CLS_SETRESOLV(class1->class_pointer);
              
          if(class1->super_class)
            {   
              Class* a_super_class 
                = objc_get_class ((char *) class1->super_class);
              
              assert (a_super_class);
              
              DEBUG_PRINTF ("making class connections for: %s\n",
                            class1->name);
              
              /* assign subclass links for superclass */
              class1->sibling_class = a_super_class->subclass_list;
              a_super_class->subclass_list = class1;
              
              /* Assign subclass links for meta class of superclass */
              if (a_super_class->class_pointer)
                {
                  class1->class_pointer->sibling_class
                    = a_super_class->class_pointer->subclass_list;
                  a_super_class->class_pointer->subclass_list 
                    = class1->class_pointer;
                }
            }
          else                  /* a root class, make its meta object */
                                /* be a subclass of Object */
            {
              class1->class_pointer->sibling_class 
                = object_class->subclass_list;
              object_class->subclass_list = class1->class_pointer;
            }
        }
    }

  /* Assign superclass links */
  for (node = hash_next (__objc_class_hash, NULL); node;
       node = hash_next (__objc_class_hash, node))
    {
      Class* class1 = node->value;
      Class* sub_class;
      for (sub_class = class1->subclass_list; sub_class;
           sub_class = sub_class->sibling_class)
        {
          sub_class->super_class = class1;
          if(CLS_ISCLASS(sub_class))
            sub_class->class_pointer->super_class = class1->class_pointer;
        }
    }
}



#define CLASSOF(c) ((c)->class_pointer)

Class*
class_pose_as (Class* impostor, Class* super_class)
{
  node_ptr node;
  Class* class1;

  if (!CLS_ISRESOLV (impostor))
    __objc_resolve_class_links ();

  /* preconditions */
  assert (impostor);
  assert (super_class);
  assert (impostor->super_class == super_class);
  assert (CLS_ISCLASS (impostor));
  assert (CLS_ISCLASS (super_class));
  assert (impostor->instance_size == super_class->instance_size);

  {
    Class **subclass = &(super_class->subclass_list);

    /* move subclasses of super_class to impostor */
    while (*subclass)
      {
	Class *nextSub = (*subclass)->sibling_class;

	if (*subclass != impostor)
	  {
	    Class *sub = *subclass;

	    /* classes */
	    sub->sibling_class = impostor->subclass_list;
	    sub->super_class = impostor;
	    impostor->subclass_list = sub;

	    /* It will happen that SUB is not a class object if it is 
	       the top of the meta class hierachy chain.  (root
	       meta-class objects inherit theit class object)  If that is
	       the case... dont mess with the meta-meta class. */ 
	    if (CLS_ISCLASS (sub))
	      {
		/* meta classes */
		CLASSOF (sub)->sibling_class = CLASSOF (impostor)->subclass_list;
		CLASSOF (sub)->super_class = CLASSOF (impostor);
		CLASSOF (impostor)->subclass_list = CLASSOF (sub);
	      }
	  }

	*subclass = nextSub;
      }

    /* set subclasses of superclass to be impostor only */
    super_class->subclass_list = impostor;
    CLASSOF (super_class)->subclass_list = CLASSOF (impostor);
    
    /* set impostor to have no sibling classes */
    impostor->sibling_class = 0;
    CLASSOF (impostor)->sibling_class = 0;
  }
  
  /* check relationship of impostor and super_class is kept. */
  assert (impostor->super_class == super_class);
  assert (CLASSOF (impostor)->super_class == CLASSOF (super_class));

  /* This is how to update the lookup table. Regardless of
     what the keys of the hashtable is, change all values that are
     suprecalss into impostor. */

  for (node = hash_next (__objc_class_hash, NULL); node;
       node = hash_next (__objc_class_hash, node))
    {
      class1 = (Class*)node->value;
      if (class1 == super_class)
	{
	  node->value = impostor; /* change hash table value */
	}
    }      

  /* next, we update the dispatch tables... */
  __objc_update_dispatch_table_for_class (CLASSOF (impostor));
  __objc_update_dispatch_table_for_class (impostor);

  return impostor;
}
  

