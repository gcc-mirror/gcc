/* GNU Objective C Runtime selector related functions
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
#include "objc/sarray.h"

/* Initial selector hash table size. Value doesnt matter much */
#define SELECTOR_HASH_SIZE 128

/* Tables mapping selector names to uid and opposite */
static struct sarray* __objc_selector_array = 0; /* uid -> name */
static cache_ptr      __objc_selector_hash  = 0; /* name -> uid */

static void register_selectors_from_list(MethodList_t);

/* Number of selectors stored in each of the above tables */
int __objc_selector_max_index = 0;

void __objc_init_selector_tables()
{
  __objc_selector_array = sarray_new (SELECTOR_HASH_SIZE, 0);
  __objc_selector_hash
    = hash_new (SELECTOR_HASH_SIZE,
		(hash_func_type) hash_string,
		(compare_func_type) compare_strings);
}  

/* This routine is given a class and records all of the methods in its class
   structure in the record table.  */
void
__objc_register_selectors_from_class (Class* class)
{
  MethodList_t method_list;

  method_list = class->methods;
  while (method_list)
    {
      register_selectors_from_list (method_list);
      method_list = method_list->method_next;
    }
}


/* This routine is given a list of methods and records each of the methods in
   the record table.  This is the routine that does the actual recording
   work.

   This one is only called for Class objects.  For categories,
   class_add_method_list is called.
   */
static void
register_selectors_from_list (MethodList_t method_list)
{
  int i = 0;
  while (i < method_list->method_count)
    {
      Method_t method = &method_list->method_list[i];
      method->method_name = sel_register_name ((char*)method->method_name);
      i += 1;
    }
}

/* return selector representing name */
SEL
sel_get_uid (const char *name)
{
  return (SEL) hash_value_for_key (__objc_selector_hash, name);
}

/* Get name of selector.  If selector is unknown, the empty string "" 
   is returned */ 
const char*
sel_get_name (SEL selector)
{
  if ((soffset_decode((sidx)selector) > 0)
      && (soffset_decode((sidx)selector) <= __objc_selector_max_index))
    return sarray_get (__objc_selector_array, (sidx) selector);
  else
    return NULL;
}

BOOL
sel_is_mapped (SEL selector)
{
  unsigned int idx = soffset_decode ((sidx)selector);
  return ((idx > 0) && (idx <= __objc_selector_max_index));
}

/* The uninstalled dispatch table */
extern struct sarray* __objc_uninstalled_dtable;

/* Store the passed selector name in the selector record and return its
   selector value (value returned by sel_get_uid). */
SEL
sel_register_name (const char *sel)
{
  SEL j;
  sidx i;

  if ((j = sel_get_uid ((const char *) sel)))
    return j;

  /* Save the selector name.  */
  __objc_selector_max_index += 1;
  i = soffset_encode(__objc_selector_max_index);

  DEBUG_PRINTF ("Record selector %s as: %#x\n", sel, i);

  sarray_at_put_safe (__objc_selector_array, i, (void *) sel);
  hash_add (&__objc_selector_hash, (void *) sel, (void *) i);

  sarray_realloc(__objc_uninstalled_dtable, __objc_selector_max_index+1);

  return (SEL) i;
}

