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
    BOOL super_is_base_class = NO;

    /* move subclasses of super_class to impostor */
    while (*subclass)
      {
	Class *nextSub = (*subclass)->sibling_class;

	/* this happens when super_class is a base class */
	if (*subclass == CLASSOF (super_class))
	  {
	    super_is_base_class = YES;
	  }
	else if (*subclass != impostor)
	  {
	    Class *sub = *subclass;

	    /* classes */
	    sub->sibling_class = impostor->subclass_list;
	    sub->super_class = impostor;
	    impostor->subclass_list = sub;
	    
	    /* meta classes */
	    CLASSOF (sub)->sibling_class = CLASSOF (impostor)->subclass_list;
	    CLASSOF (sub)->super_class = CLASSOF (impostor);
	    CLASSOF (impostor)->subclass_list = CLASSOF (sub);
	  }

	*subclass = nextSub;
      }

    /* set subclasses of superclass to be impostor only */
    super_class->subclass_list = impostor;
    CLASSOF (super_class)->subclass_list = CLASSOF (impostor);
    
    /* set impostor to have no sibling classes */
    impostor->sibling_class = 0;
    CLASSOF (impostor)->sibling_class = 0;

    /* impostor has a sibling... */
    if (super_is_base_class)
      {
	CLASSOF (super_class)->sibling_class = 0;
	impostor->sibling_class = CLASSOF (super_class);
      }
  }
  
  /* check relationship of impostor and super_class */
  assert (impostor->super_class == super_class);
  assert (CLASSOF (impostor)->super_class == CLASSOF (super_class));

  /* by now, the re-organization of the class hierachy 
     is done.  We only need to update various tables. */

  /* First, we change the names in the hash table.
     This will change the behavior of objc_get_class () */
  {
    char* buffer = (char*) __objc_xmalloc(strlen (super_class->name) + 2);

    strcpy (buffer+1, super_class->name);
    buffer[0] = '*';

    /* keep on prepending '*' until the name is unique */
    while (hash_value_for_key (__objc_class_hash, buffer))
      {
	char *bbuffer = (char*) __objc_xmalloc (strlen (buffer)+2);

	strcpy (bbuffer+1, buffer);
	bbuffer[0] = '*';
	free (buffer);
	buffer = bbuffer;
      }

    hash_remove (__objc_class_hash, super_class->name);
    hash_add (&__objc_class_hash, buffer, super_class);
    hash_add (&__objc_class_hash, super_class->name, impostor);

    /* Note that -name and +name will still respond with
       the same strings as before.  This way any
       -isKindOfGivenName: will always work.         */
  }

  /* next, we update the dispatch tables... */
  {
    Class *subclass;

    for (subclass = impostor->subclass_list;
	 subclass; subclass = subclass->sibling_class)
      {
	/* we use the opportunity to check what we did */
	assert (subclass->super_class == impostor);
	assert (CLASSOF (subclass)->super_class == CLASSOF (impostor));

	__objc_update_dispatch_table_for_class (CLASSOF (subclass));
	__objc_update_dispatch_table_for_class (subclass);
      }
  }

  return impostor;
}
  
