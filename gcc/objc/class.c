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


/* This is a incomplete implementation of posing.   This function does the
   bulk of the work but does not initialize the class method caches.  That is
   a run-time specific operation.

I implement posing by hiding SUPER_CLASS, creating new class and meta class
   structures, initializing it with IMPOSTOR, and changing it such that it is
   identified as SUPER_CLASS. SUPER_CLASS remains in the hierarchy but is
   inaccessible by the means. The class hierarchy is then re arranged such
   that all of the subclasses of SUPER_CLASS now inherit from the new class
   structures -- except the impostor itself. The only dramatic effect on the
   application is that subclasses of SUPER_CLASS cannot do a [ ....
   super_class ] and expect their real super class. */
Class*
class_pose_as (Class* impostor, Class* super_class)
{
  Class* new_class = (Class*) __objc_xcalloc (1, sizeof (Class));
  MetaClass* new_meta_class =
    (MetaClass*) __objc_xmalloc(sizeof (MetaClass));
  char *new_name = (char *)__objc_xmalloc ((size_t)strlen ((char*)super_class->name) + 12);

  /* We must know the state of the hierachy.  Do initial setup if needed */
  if(!CLS_ISRESOLV(impostor))
    __objc_resolve_class_links();

  assert (new_class);
  assert (new_meta_class);
  assert (new_name);

  assert (CLS_ISCLASS(impostor));
  assert (CLS_ISCLASS(super_class));

  assert (impostor->instance_size == super_class->instance_size);

  /* Create the impostor class.  */
  new_class->class_pointer = new_meta_class;
  new_class->super_class = super_class;
  new_class->name = super_class->name;
  new_class->version = super_class->version;
  new_class->info = super_class->info;
  new_class->instance_size = super_class->instance_size;
  new_class->ivars = super_class->ivars;
  new_class->methods = impostor->methods;
  new_class->dtable = impostor->dtable;

  /* Create the impostor meta class.  */
  new_meta_class->class_pointer = super_class->class_pointer->class_pointer;
  new_meta_class->super_class = super_class->class_pointer->super_class;
  new_meta_class->name = super_class->class_pointer->name;
  new_meta_class->version = super_class->class_pointer->version;
  new_meta_class->info = super_class->class_pointer->info;
  new_meta_class->instance_size = super_class->class_pointer->instance_size;
  new_meta_class->ivars = super_class->class_pointer->ivars;
  new_meta_class->methods = impostor->class_pointer->methods;
  new_meta_class->dtable = impostor->class_pointer->dtable;

  /* Now change super/subclass links of all related classes.  This is rather
     complex, since we have both super_class link, and subclass_list for the
     involved classes. */
  {
    Class* *classpp;
    MetaClass* *metaclasspp;

    /* Remove impostor from subclass list of super_class */
    for (classpp = &(super_class->subclass_list);
         *classpp;
         classpp = &((*classpp)->sibling_class))
      {
        if (*classpp == impostor)
          *classpp = (*classpp)->sibling_class;
        if (*classpp == 0)
          break;
      }

    /* Do the same for the meta classes */

    for (metaclasspp = &(super_class->class_pointer->subclass_list);
         *metaclasspp;
         metaclasspp = &((*metaclasspp)->sibling_class))
      {
        if (*metaclasspp == impostor->class_pointer)
          *metaclasspp = (*metaclasspp)->sibling_class;
        if (*metaclasspp == 0)
          break;
      }

    /* From the loop above, classpp now points to the sibling_class entry */
    /* of the last element in the list of subclasses for super_class */

    /* Append the subclass list of impostor to the subclass list of */
    /* superclass, and excange those two and set subclass of */
    /* super_class to be impostor only */

    *classpp = impostor->subclass_list;
    new_class->subclass_list = super_class->subclass_list;
    super_class->subclass_list = new_class;
    new_class->sibling_class = 0;

    /* Do the same thing for the meta classes */
    *metaclasspp = impostor->class_pointer->subclass_list;
    new_meta_class->subclass_list = super_class->class_pointer->subclass_list;
    super_class->class_pointer->subclass_list = new_meta_class;
    new_meta_class->sibling_class = 0;

    /* Update superclass links for all subclasses of new_class */
    for (classpp = &(new_class->subclass_list); *classpp;
         classpp = &((*classpp)->sibling_class))
      (*classpp)->super_class = new_class;

    for (metaclasspp = &(new_meta_class->subclass_list); *metaclasspp;
         metaclasspp = &((*metaclasspp)->sibling_class))
      (*metaclasspp)->super_class = new_meta_class;

  }

  /* Delete the class from the hash table, change its name so that it can no
     longer be found, then place it back into the hash table using its new
     name.
  
  Don't worry about the class number.  It is already assigned.
     memory is lost with the hash key.) */
  hash_remove (__objc_class_hash, super_class->name);
  sprintf (new_name, "%s*", super_class->name);
  super_class->name = new_name;
  super_class->class_pointer->name = new_name;
  hash_add (&__objc_class_hash, super_class->name, super_class);

  /* Place the impostor class in class hash table and assign it a class
     number.  */
  __objc_add_class_to_hash (new_class);

  /* Now update dispatch tables for new_class and it's subclasses */
  __objc_update_dispatch_table_for_class ((Class*) new_meta_class);
  __objc_update_dispatch_table_for_class (new_class);

  return new_class;
}

