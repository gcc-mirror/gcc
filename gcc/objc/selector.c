/* GNU Objective C Runtime selector related functions
   Copyright (C) 1993, 1995 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup

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
#include "objc/sarray.h"
#include "encoding.h"

/* Initial selector hash table size. Value doesn't matter much */
#define SELECTOR_HASH_SIZE 128

/* Tables mapping selector names to uid and opposite */
static struct sarray* __objc_selector_array = 0; /* uid -> sel */
static struct sarray* __objc_selector_names = 0; /* uid -> name */
static cache_ptr      __objc_selector_hash  = 0; /* name -> uid */

static void register_selectors_from_list(MethodList_t);

/* Number of selectors stored in each of the above tables */
int __objc_selector_max_index = 0;

void __objc_init_selector_tables()
{
  __objc_selector_array = sarray_new (SELECTOR_HASH_SIZE, 0);
  __objc_selector_names = sarray_new (SELECTOR_HASH_SIZE, 0);
  __objc_selector_hash
    = hash_new (SELECTOR_HASH_SIZE,
		(hash_func_type) hash_string,
		(compare_func_type) compare_strings);
}  

/* This routine is given a class and records all of the methods in its class
   structure in the record table.  */
void
__objc_register_selectors_from_class (Class class)
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
      method->method_name 
	= sel_register_typed_name ((const char*)method->method_name, 
				     method->method_types);
      i += 1;
    }
}


/* Returns YES iff t1 and t2 have same method types, but we ignore
   the argframe layout */
BOOL
sel_types_match (const char* t1, const char* t2)
{
  if (!t1 || !t2)
    return NO;
  while (*t1 && *t2)
    {
      if (*t1 == '+') t1++;
      if (*t2 == '+') t2++;
      while (isdigit(*t1)) t1++;
      while (isdigit(*t2)) t2++;
      /* xxx Remove these next two lines when qualifiers are put in
	 all selectors, not just Protocol selectors. */
      t1 = objc_skip_type_qualifiers(t1);
      t2 = objc_skip_type_qualifiers(t2);
      if (!*t1 && !*t2)
	return YES;
      if (*t1 != *t2)
	return NO;
      t1++;
      t2++;
    }
  return NO;
}

/* return selector representing name */
SEL
sel_get_typed_uid (const char *name, const char *types)
{
  struct objc_list *l;
  sidx i;

  i = (sidx) hash_value_for_key (__objc_selector_hash, name);
  if (i == 0)
    return 0;

  for (l = (struct objc_list*)sarray_get (__objc_selector_array, i);
       l; l = l->tail)
    {
      SEL s = (SEL)l->head;
      if (types == 0 || s->sel_types == 0)
	{
	  if (s->sel_types == types)
	    {
	      return s;
	    }
	}
      else if (sel_types_match (s->sel_types, types))
	{
	  return s;
	}
    }

  return 0;
}

/* Return selector representing name; prefer a selector with non-NULL type */
SEL
sel_get_any_typed_uid (const char *name)
{
  struct objc_list *l;
  sidx i;
  SEL s;

  i = (sidx) hash_value_for_key (__objc_selector_hash, name);
  if (i == 0)
    return 0;

  for (l = (struct objc_list*)sarray_get (__objc_selector_array, i);
       l; l = l->tail)
    {
      s = (SEL) l->head;
      if (s->sel_types)
	return s;
    }

  return s;
}

/* return selector representing name */
SEL
sel_get_any_uid (const char *name)
{
  struct objc_list *l;
  sidx i;

  i = (sidx) hash_value_for_key (__objc_selector_hash, name);
  if (soffset_decode (i) == 0)
    return 0;

  l = (struct objc_list*)sarray_get (__objc_selector_array, i);
  if (l == 0)
    return 0;

  return (SEL)l->head;
}

/* return selector representing name */
SEL
sel_get_uid (const char *name)
{
  return sel_register_typed_name (name, 0);
}

/* Get name of selector.  If selector is unknown, the empty string "" 
   is returned */ 
const char*
sel_get_name (SEL selector)
{
  if ((soffset_decode((sidx)selector->sel_id) > 0)
      && (soffset_decode((sidx)selector->sel_id) <= __objc_selector_max_index))
    return sarray_get (__objc_selector_names, (sidx) selector->sel_id);
  else
    return 0;
}

BOOL
sel_is_mapped (SEL selector)
{
  unsigned int idx = soffset_decode ((sidx)selector->sel_id);
  return ((idx > 0) && (idx <= __objc_selector_max_index));
}


const char*
sel_get_type (SEL selector)
{
  if (selector)
    return selector->sel_types;
  else
    return 0;
}

/* The uninstalled dispatch table */
extern struct sarray* __objc_uninstalled_dtable;

/* Store the passed selector name in the selector record and return its
   selector value (value returned by sel_get_uid). */
SEL
__sel_register_typed_name (const char *name, const char *types, 
			   struct objc_selector *orig)
{
  struct objc_selector* j;
  sidx i;
  struct objc_list *l;

  i = (sidx) hash_value_for_key (__objc_selector_hash, name);
  if (soffset_decode (i) != 0)
    {
      for (l = (struct objc_list*)sarray_get (__objc_selector_array, i);
	   l; l = l->tail)
	{
	  SEL s = (SEL)l->head;
	  if (types == 0 || s->sel_types == 0)
	    {
	      if (s->sel_types == types)
		{
		  if (orig)
		    {
		      orig->sel_id = (void*)i;
		      return orig;
		    }
		  else
		    return s;
		}
	    }
	  else if (!strcmp (s->sel_types, types))
	    {
	      if (orig)
		{
		  orig->sel_id = (void*)i;
		  return orig;
		}
	      else
		return s;
	    }
	}
      if (orig)
	j = orig;
      else
	j = __objc_xmalloc (sizeof (struct objc_selector));

      j->sel_id = (void*)i;
      j->sel_types = (const char*)types;
      l = (struct objc_list*)sarray_get (__objc_selector_array, i);
    }
  else
    {
      __objc_selector_max_index += 1;
      i = soffset_encode(__objc_selector_max_index);
      if (orig)
	j = orig;
      else
	j = __objc_xmalloc (sizeof (struct objc_selector));
	
      j->sel_id = (void*)i;
      j->sel_types = (const char*)types;
      l = 0;
    }

  DEBUG_PRINTF ("Record selector %s[%s] as: %ld\n", name, types, 
		soffset_decode (i));
  
  {
    int is_new = (l == 0);
    l = list_cons ((void*)j, l);
    sarray_at_put_safe (__objc_selector_names, i, (void *) name);
    sarray_at_put_safe (__objc_selector_array, i, (void *) l);
    if (is_new)
      hash_add (&__objc_selector_hash, (void *) name, (void *) i);
  }

  sarray_realloc(__objc_uninstalled_dtable, __objc_selector_max_index+1);

  return (SEL) j;
}

SEL
sel_register_name (const char *name)
{
  return __sel_register_typed_name (name, 0, 0);
}

SEL
sel_register_typed_name (const char *name, const char *type)
{
  return __sel_register_typed_name (name, type, 0);
}

