/* malloc.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 2003 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Related Modules:
      None

   Description:
      Fast pool-based memory allocation.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "malloc.h"

/* Externals defined here.  */

struct _malloc_root_ malloc_root_
=
{
  {
    &malloc_root_.malloc_pool_image_,
    &malloc_root_.malloc_pool_image_,
    (mallocPool) &malloc_root_.malloc_pool_image_.eldest,
    (mallocPool) &malloc_root_.malloc_pool_image_.eldest,
    (mallocArea_) &malloc_root_.malloc_pool_image_.first,
    (mallocArea_) &malloc_root_.malloc_pool_image_.first,
    0,
#if MALLOC_DEBUG
    0, 0, 0, 0, 0, 0, 0, { '/' }
#else
    { 0 }
#endif
  },
};

/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */

static void *malloc_reserve_ = NULL;	/* For crashes. */
#if MALLOC_DEBUG
static const char *const malloc_types_[] =
{"KS", "KSR", "NF", "NFR", "US", "USR"};
#endif

/* Static functions (internal). */

static void malloc_kill_area_ (mallocPool pool, mallocArea_ a);
#if MALLOC_DEBUG
static void malloc_verify_area_ (mallocPool pool, mallocArea_ a);
#endif

/* Internal macros. */

#if MALLOC_DEBUG
#define malloc_kill_(ptr,s) do {memset((ptr),127,(s));free((ptr));} while(0)
#else
#define malloc_kill_(ptr,s) free((ptr))
#endif

/* malloc_kill_area_ -- Kill storage area and its object

   malloc_kill_area_(mallocPool pool,mallocArea_ area);

   Does the actual killing of a storage area.  */

static void
malloc_kill_area_ (mallocPool pool UNUSED, mallocArea_ a)
{
#if MALLOC_DEBUG
  assert (strcmp (a->name, ((char *) (a->where)) + a->size) == 0);
#endif
  malloc_kill_ (a->where - sizeof(mallocArea_*), a->size);
  a->next->previous = a->previous;
  a->previous->next = a->next;
#if MALLOC_DEBUG
  pool->freed += a->size;
  pool->frees++;
#endif
  
  malloc_kill_ (a,
		offsetof (struct _malloc_area_, name)
		+ strlen (a->name) + 1);
}

/* malloc_verify_area_ -- Verify storage area and its object

   malloc_verify_area_(mallocPool pool,mallocArea_ area);

   Does the actual verifying of a storage area.  */

#if MALLOC_DEBUG
static void
malloc_verify_area_ (mallocPool pool UNUSED, mallocArea_ a UNUSED)
{
  mallocSize s = a->size;

  assert (strcmp (a->name, ((char *) (a->where)) + s) == 0);
}
#endif

/* malloc_init -- Initialize malloc cluster

   malloc_init();

   Call malloc_init before you do anything else.  */

void
malloc_init (void)
{
  if (malloc_reserve_ != NULL)
    return;
  malloc_reserve_ = xmalloc (20 * 1024); /* In case of crash, free this first. */
}

/* malloc_pool_display -- Display a pool

   mallocPool p;
   malloc_pool_display(p);

   Displays information associated with the pool and its subpools.  */

void
malloc_pool_display (mallocPool p UNUSED)
{
#if MALLOC_DEBUG
  mallocPool q;
  mallocArea_ a;

  fprintf (dmpout, "Pool \"%s\": bytes allocated=%lu, freed=%lu, old sizes=%lu, new sizes\
=%lu,\n   allocations=%lu, frees=%lu, resizes=%lu, uses=%lu\n   Subpools:\n",
	   p->name, p->allocated, p->freed, p->old_sizes, p->new_sizes, p->allocations,
	   p->frees, p->resizes, p->uses);

  for (q = p->eldest; q != (mallocPool) & p->eldest; q = q->next)
    fprintf (dmpout, "      \"%s\"\n", q->name);

  fprintf (dmpout, "   Storage areas:\n");

  for (a = p->first; a != (mallocArea_) & p->first; a = a->next)
    {
      fprintf (dmpout, "      ");
      malloc_display_ (a);
    }
#endif
}

/* malloc_pool_kill -- Destroy a pool

   mallocPool p;
   malloc_pool_kill(p);

   Releases all storage associated with the pool and its subpools.  */

void
malloc_pool_kill (mallocPool p)
{
  mallocPool q;
  mallocArea_ a;

  if (--p->uses != 0)
    return;

#if 0
  malloc_pool_display (p);
#endif

  assert (p->next->previous == p);
  assert (p->previous->next == p);

  /* Kill off all the subpools. */

  while ((q = p->eldest) != (mallocPool) &p->eldest)
    {
      q->uses = 1;		/* Force the kill. */
      malloc_pool_kill (q);
    }

  /* Now free all the storage areas. */

  while ((a = p->first) != (mallocArea_) & p->first)
    {
      malloc_kill_area_ (p, a);
    }

  /* Now remove from list of sibling pools. */

  p->next->previous = p->previous;
  p->previous->next = p->next;

  /* Finally, free the pool itself. */

  malloc_kill_ (p,
		offsetof (struct _malloc_pool_, name)
		+ strlen (p->name) + 1);
}

/* malloc_pool_new -- Make a new pool

   mallocPool p;
   p = malloc_pool_new("My new pool",malloc_pool_image(),1024);

   Makes a new pool with the given name and default new-chunk allocation.  */

mallocPool
malloc_pool_new (const char *name, mallocPool parent,
		 unsigned long chunks UNUSED)
{
  mallocPool p;

  if (parent == NULL)
    parent = malloc_pool_image ();

  p = malloc_new_ (offsetof (struct _malloc_pool_, name)
		   + (MALLOC_DEBUG ? strlen (name) + 1 : 0));
  p->next = (mallocPool) &(parent->eldest);
  p->previous = parent->youngest;
  parent->youngest->next = p;
  parent->youngest = p;
  p->eldest = (mallocPool) &(p->eldest);
  p->youngest = (mallocPool) &(p->eldest);
  p->first = (mallocArea_) &(p->first);
  p->last = (mallocArea_) &(p->first);
  p->uses = 1;
#if MALLOC_DEBUG
  p->allocated = p->freed = p->old_sizes = p->new_sizes = p->allocations
    = p->frees = p->resizes = 0;
  strcpy (p->name, name);
#endif
  return p;
}

/* malloc_pool_use -- Use an existing pool

   mallocPool p;
   p = malloc_pool_new(pool);

   Increments use count for pool; means a matching malloc_pool_kill must
   be performed before a subsequent one will actually kill the pool.  */

mallocPool
malloc_pool_use (mallocPool pool)
{
  ++pool->uses;
  return pool;
}

/* malloc_display_ -- Display info on a mallocArea_

   mallocArea_ a;
   malloc_display_(a);

   Simple.  */

void
malloc_display_ (mallocArea_ a UNUSED)
{
#if MALLOC_DEBUG
  fprintf (dmpout, "At %08lX, size=%" mallocSize_f "u, type=%s, \"%s\"\n",
	(unsigned long) a->where, a->size, malloc_types_[a->type], a->name);
#endif
}

/* malloc_find_inpool_ -- Find mallocArea_ for object in pool

   mallocPool pool;
   void *ptr;
   mallocArea_ a;
   a = malloc_find_inpool_(pool,ptr);

   Search for object in list of mallocArea_s, die if not found.	 */

mallocArea_
malloc_find_inpool_ (mallocPool pool, void *ptr)
{
  mallocArea_ *t;
  t = (mallocArea_ *) (ptr - sizeof(mallocArea_));
  return *t;
}

/* malloc_kill_inpool_ -- Kill object

   malloc_kill_inpool_(NULL,MALLOC_typeUS_,ptr,size_in_bytes);

   Find the mallocArea_ for the pointer, make sure the type is proper, and
   kill both of them.  */

void
malloc_kill_inpool_ (mallocPool pool, mallocType_ type UNUSED,
		     void *ptr, mallocSize s UNUSED)
{
  mallocArea_ a;

  if (pool == NULL)
    pool = malloc_pool_image ();

#if MALLOC_DEBUG
  assert ((pool == malloc_pool_image ())
	  || malloc_pool_find_ (pool, malloc_pool_image ()));
#endif

  a = malloc_find_inpool_ (pool, ptr);
#if MALLOC_DEBUG
  assert (a->type == type);
  if ((type != MALLOC_typeUS_) && (type != MALLOC_typeUSR_))
    assert (a->size == s);
#endif
  malloc_kill_area_ (pool, a);
}

/* malloc_new_ -- Allocate new object, die if unable

   ptr = malloc_new_(size_in_bytes);

   Call malloc, bomb if it returns NULL.  */

void *
malloc_new_ (mallocSize s)
{
  void *ptr;
  unsigned ss = s;

#if MALLOC_DEBUG && 0
  assert (s == (mallocSize) ss);/* Else alloc is too big for this
				   library/sys. */
#endif

  ptr = xmalloc (ss);
#if MALLOC_DEBUG
  memset (ptr, 126, ss);	/* Catch some kinds of errors more
				   quickly/reliably. */
#endif
  return ptr;
}

/* malloc_new_inpool_ -- Allocate new object, die if unable

   ptr = malloc_new_inpool_(NULL,MALLOC_typeUS_,"object",size_in_bytes);

   Allocate the structure and allocate a mallocArea_ to describe it, then
   add it to the list of mallocArea_s for the pool.  */

void *
malloc_new_inpool_ (mallocPool pool, mallocType_ type, const char *name, mallocSize s)
{
  void *ptr;
  mallocArea_ a;
  unsigned short i;
  mallocArea_ *temp;

  if (pool == NULL)
    pool = malloc_pool_image ();

#if MALLOC_DEBUG
  assert ((pool == malloc_pool_image ())
	  || malloc_pool_find_ (pool, malloc_pool_image ()));
#endif

  ptr = malloc_new_ (sizeof(mallocArea_*) + s + (i = (MALLOC_DEBUG ? strlen (name) + 1 : 0)));
#if MALLOC_DEBUG
  strcpy (((char *) (ptr)) + s, name);
#endif
  a = malloc_new_ (offsetof (struct _malloc_area_, name) + i);
  temp = (mallocArea_ *) ptr;
  *temp = a; 
  ptr = ptr + sizeof(mallocArea_*);
  switch (type)
    {				/* A little optimization to speed up killing
				   of non-permanent stuff. */
    case MALLOC_typeKP_:
    case MALLOC_typeKPR_:
      a->next = (mallocArea_) &pool->first;
      break;

    default:
      a->next = pool->first;
      break;
    }
  a->previous = a->next->previous;
  a->next->previous = a;
  a->previous->next = a;
  a->where = ptr;
#if MALLOC_DEBUG
  a->size = s;
  a->type = type;
  strcpy (a->name, name);
  pool->allocated += s;
  pool->allocations++;
#endif
  return ptr;
}

/* malloc_new_zinpool_ -- Allocate new zeroed object, die if unable

   ptr = malloc_new_zinpool_(NULL,MALLOC_typeUS_,"object",size_in_bytes,0);

   Like malloc_new_inpool_, but zeros out all the bytes in the area (assuming
   you pass it a 0).  */

void *
malloc_new_zinpool_ (mallocPool pool, mallocType_ type, const char *name, mallocSize s,
		     int z)
{
  void *ptr;

  ptr = malloc_new_inpool_ (pool, type, name, s);
  memset (ptr, z, s);
  return ptr;
}

/* malloc_pool_find_ -- See if pool is a descendant of another pool

   if (malloc_pool_find_(target_pool,parent_pool)) ...;

   Recursive descent on each of the children of the parent pool, after
   first checking the children themselves.  */

char
malloc_pool_find_ (mallocPool pool, mallocPool parent)
{
  mallocPool p;

  for (p = parent->eldest; p != (mallocPool) & parent->eldest; p = p->next)
    {
      if ((p == pool) || malloc_pool_find_ (pool, p))
	return 1;
    }
  return 0;
}

/* malloc_resize_inpool_ -- Resize existing object in pool

   ptr = malloc_resize_inpool_(NULL,MALLOC_typeUSR_,ptr,new_size,old_size);

   Find the object's mallocArea_, check it out, then do the resizing.  */

void *
malloc_resize_inpool_ (mallocPool pool, mallocType_ type UNUSED,
		       void *ptr, mallocSize ns, mallocSize os UNUSED)
{
  mallocArea_ a;
  mallocArea_ *temp;

  if (pool == NULL)
    pool = malloc_pool_image ();

#if MALLOC_DEBUG
  assert ((pool == malloc_pool_image ())
	  || malloc_pool_find_ (pool, malloc_pool_image ()));
#endif

  a = malloc_find_inpool_ (pool, ptr);
#if MALLOC_DEBUG
  assert (a->type == type);
  if ((type == MALLOC_typeKSR_) || (type == MALLOC_typeKPR_))
    assert (a->size == os);
  assert (strcmp (a->name, ((char *) (ptr)) + os) == 0);
#endif
  ptr = malloc_resize_ (ptr - sizeof(mallocArea_*), sizeof(mallocArea_*) + ns + (MALLOC_DEBUG ? strlen (a->name) + 1: 0));
  temp = (mallocArea_ *) ptr;
  *temp = a;
  ptr = ptr + sizeof(mallocArea_*);
  a->where = ptr;
#if MALLOC_DEBUG
  a->size = ns;
  strcpy (((char *) (ptr)) + ns, a->name);
  pool->old_sizes += os;
  pool->new_sizes += ns;
  pool->resizes++;
#endif
  return ptr;
}

/* malloc_resize_ -- Reallocate object, die if unable

   ptr = malloc_resize_(ptr,size_in_bytes);

   Call realloc, bomb if it returns NULL.  */

void *
malloc_resize_ (void *ptr, mallocSize s)
{
  int ss = s;

#if MALLOC_DEBUG && 0
  assert (s == (mallocSize) ss);/* Too big if failure here. */
#endif

  ptr = xrealloc (ptr, ss);
  return ptr;
}

/* malloc_verify_inpool_ -- Verify object

   Find the mallocArea_ for the pointer, make sure the type is proper, and
   verify both of them.  */

void
malloc_verify_inpool_ (mallocPool pool UNUSED, mallocType_ type UNUSED,
		       void *ptr UNUSED, mallocSize s UNUSED)
{
#if MALLOC_DEBUG
  mallocArea_ a;

  if (pool == NULL)
    pool = malloc_pool_image ();

  assert ((pool == malloc_pool_image ())
	  || malloc_pool_find_ (pool, malloc_pool_image ()));

  a = malloc_find_inpool_ (pool, ptr);
  assert (a->type == type);
  if ((type != MALLOC_typeUS_) && (type != MALLOC_typeUSR_))
    assert (a->size == s);
  malloc_verify_area_ (pool, a);
#endif
}
