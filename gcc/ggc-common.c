/* Simple garbage collection for the GNU compiler.
   Copyright (C) 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Generic garbage collection (GC) functions and data, not specific to
   any particular GC implementation.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hashtab.h"
#include "ggc.h"
#include "toplev.h"
#include "params.h"
#include "hosthooks.h"

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_MMAP_FILE
# include <sys/mman.h>
# ifdef HAVE_MINCORE
/* This is on Solaris.  */
#  include <sys/types.h> 
# endif
#endif

#ifndef MAP_FAILED
# define MAP_FAILED ((void *)-1)
#endif

#ifdef ENABLE_VALGRIND_CHECKING
# ifdef HAVE_VALGRIND_MEMCHECK_H
#  include <valgrind/memcheck.h>
# elif defined HAVE_MEMCHECK_H
#  include <memcheck.h>
# else
#  include <valgrind.h>
# endif
#else
/* Avoid #ifdef:s when we can help it.  */
#define VALGRIND_DISCARD(x)
#endif

/* Statistics about the allocation.  */
static ggc_statistics *ggc_stats;

struct traversal_state;

static int ggc_htab_delete (void **, void *);
static hashval_t saving_htab_hash (const void *);
static int saving_htab_eq (const void *, const void *);
static int call_count (void **, void *);
static int call_alloc (void **, void *);
static int compare_ptr_data (const void *, const void *);
static void relocate_ptrs (void *, void *);
static void write_pch_globals (const struct ggc_root_tab * const *tab,
			       struct traversal_state *state);
static double ggc_rlimit_bound (double);

/* Maintain global roots that are preserved during GC.  */

/* Process a slot of an htab by deleting it if it has not been marked.  */

static int
ggc_htab_delete (void **slot, void *info)
{
  const struct ggc_cache_tab *r = (const struct ggc_cache_tab *) info;

  if (! (*r->marked_p) (*slot))
    htab_clear_slot (*r->base, slot);
  else
    (*r->cb) (*slot);

  return 1;
}

/* Iterate through all registered roots and mark each element.  */

void
ggc_mark_roots (void)
{
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  const struct ggc_cache_tab *const *ct;
  const struct ggc_cache_tab *cti;
  size_t i;

  for (rt = gt_ggc_deletable_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride);

  for (rt = gt_ggc_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	(*rti->cb)(*(void **)((char *)rti->base + rti->stride * i));

  ggc_mark_stringpool ();

  /* Now scan all hash tables that have objects which are to be deleted if
     they are not already marked.  */
  for (ct = gt_ggc_cache_rtab; *ct; ct++)
    for (cti = *ct; cti->base != NULL; cti++)
      if (*cti->base)
	{
	  ggc_set_mark (*cti->base);
	  htab_traverse_noresize (*cti->base, ggc_htab_delete, (void *) cti);
	  ggc_set_mark ((*cti->base)->entries);
	}
}

/* Allocate a block of memory, then clear it.  */
void *
ggc_alloc_cleared (size_t size)
{
  void *buf = ggc_alloc (size);
  memset (buf, 0, size);
  return buf;
}

/* Resize a block of memory, possibly re-allocating it.  */
void *
ggc_realloc (void *x, size_t size)
{
  void *r;
  size_t old_size;

  if (x == NULL)
    return ggc_alloc (size);

  old_size = ggc_get_size (x);
  if (size <= old_size)
    {
      /* Mark the unwanted memory as unaccessible.  We also need to make
	 the "new" size accessible, since ggc_get_size returns the size of
	 the pool, not the size of the individually allocated object, the
	 size which was previously made accessible.  Unfortunately, we
	 don't know that previously allocated size.  Without that
	 knowledge we have to lose some initialization-tracking for the
	 old parts of the object.  An alternative is to mark the whole
	 old_size as reachable, but that would lose tracking of writes
	 after the end of the object (by small offsets).  Discard the
	 handle to avoid handle leak.  */
      VALGRIND_DISCARD (VALGRIND_MAKE_NOACCESS ((char *) x + size,
						old_size - size));
      VALGRIND_DISCARD (VALGRIND_MAKE_READABLE (x, size));
      return x;
    }

  r = ggc_alloc (size);

  /* Since ggc_get_size returns the size of the pool, not the size of the
     individually allocated object, we'd access parts of the old object
     that were marked invalid with the memcpy below.  We lose a bit of the
     initialization-tracking since some of it may be uninitialized.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_READABLE (x, old_size));

  memcpy (r, x, old_size);

  /* The old object is not supposed to be used anymore.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_NOACCESS (x, old_size));

  return r;
}

/* Like ggc_alloc_cleared, but performs a multiplication.  */
void *
ggc_calloc (size_t s1, size_t s2)
{
  return ggc_alloc_cleared (s1 * s2);
}

/* These are for splay_tree_new_ggc.  */
void *
ggc_splay_alloc (int sz, void *nl)
{
  if (nl != NULL)
    abort ();
  return ggc_alloc (sz);
}

void
ggc_splay_dont_free (void * x ATTRIBUTE_UNUSED, void *nl)
{
  if (nl != NULL)
    abort ();
}

/* Print statistics that are independent of the collector in use.  */
#define SCALE(x) ((unsigned long) ((x) < 1024*10 \
		  ? (x) \
		  : ((x) < 1024*1024*10 \
		     ? (x) / 1024 \
		     : (x) / (1024*1024))))
#define LABEL(x) ((x) < 1024*10 ? ' ' : ((x) < 1024*1024*10 ? 'k' : 'M'))

void
ggc_print_common_statistics (FILE *stream ATTRIBUTE_UNUSED,
			     ggc_statistics *stats)
{
  /* Set the pointer so that during collection we will actually gather
     the statistics.  */
  ggc_stats = stats;

  /* Then do one collection to fill in the statistics.  */
  ggc_collect ();

  /* At present, we don't really gather any interesting statistics.  */

  /* Don't gather statistics any more.  */
  ggc_stats = NULL;
}

/* Functions for saving and restoring GCable memory to disk.  */

static htab_t saving_htab;

struct ptr_data
{
  void *obj;
  void *note_ptr_cookie;
  gt_note_pointers note_ptr_fn;
  gt_handle_reorder reorder_fn;
  size_t size;
  void *new_addr;
};

#define POINTER_HASH(x) (hashval_t)((long)x >> 3)

/* Register an object in the hash table.  */

int
gt_pch_note_object (void *obj, void *note_ptr_cookie,
		    gt_note_pointers note_ptr_fn)
{
  struct ptr_data **slot;

  if (obj == NULL || obj == (void *) 1)
    return 0;

  slot = (struct ptr_data **)
    htab_find_slot_with_hash (saving_htab, obj, POINTER_HASH (obj),
			      INSERT);
  if (*slot != NULL)
    {
      if ((*slot)->note_ptr_fn != note_ptr_fn
	  || (*slot)->note_ptr_cookie != note_ptr_cookie)
	abort ();
      return 0;
    }

  *slot = xcalloc (sizeof (struct ptr_data), 1);
  (*slot)->obj = obj;
  (*slot)->note_ptr_fn = note_ptr_fn;
  (*slot)->note_ptr_cookie = note_ptr_cookie;
  if (note_ptr_fn == gt_pch_p_S)
    (*slot)->size = strlen (obj) + 1;
  else
    (*slot)->size = ggc_get_size (obj);
  return 1;
}

/* Register an object in the hash table.  */

void
gt_pch_note_reorder (void *obj, void *note_ptr_cookie,
		     gt_handle_reorder reorder_fn)
{
  struct ptr_data *data;

  if (obj == NULL || obj == (void *) 1)
    return;

  data = htab_find_with_hash (saving_htab, obj, POINTER_HASH (obj));
  if (data == NULL
      || data->note_ptr_cookie != note_ptr_cookie)
    abort ();

  data->reorder_fn = reorder_fn;
}

/* Hash and equality functions for saving_htab, callbacks for htab_create.  */

static hashval_t
saving_htab_hash (const void *p)
{
  return POINTER_HASH (((struct ptr_data *)p)->obj);
}

static int
saving_htab_eq (const void *p1, const void *p2)
{
  return ((struct ptr_data *)p1)->obj == p2;
}

/* Handy state for the traversal functions.  */

struct traversal_state
{
  FILE *f;
  struct ggc_pch_data *d;
  size_t count;
  struct ptr_data **ptrs;
  size_t ptrs_i;
};

/* Callbacks for htab_traverse.  */

static int
call_count (void **slot, void *state_p)
{
  struct ptr_data *d = (struct ptr_data *)*slot;
  struct traversal_state *state = (struct traversal_state *)state_p;

  ggc_pch_count_object (state->d, d->obj, d->size, d->note_ptr_fn == gt_pch_p_S);
  state->count++;
  return 1;
}

static int
call_alloc (void **slot, void *state_p)
{
  struct ptr_data *d = (struct ptr_data *)*slot;
  struct traversal_state *state = (struct traversal_state *)state_p;

  d->new_addr = ggc_pch_alloc_object (state->d, d->obj, d->size, d->note_ptr_fn == gt_pch_p_S);
  state->ptrs[state->ptrs_i++] = d;
  return 1;
}

/* Callback for qsort.  */

static int
compare_ptr_data (const void *p1_p, const void *p2_p)
{
  struct ptr_data *p1 = *(struct ptr_data *const *)p1_p;
  struct ptr_data *p2 = *(struct ptr_data *const *)p2_p;
  return (((size_t)p1->new_addr > (size_t)p2->new_addr)
	  - ((size_t)p1->new_addr < (size_t)p2->new_addr));
}

/* Callbacks for note_ptr_fn.  */

static void
relocate_ptrs (void *ptr_p, void *state_p)
{
  void **ptr = (void **)ptr_p;
  struct traversal_state *state ATTRIBUTE_UNUSED
    = (struct traversal_state *)state_p;
  struct ptr_data *result;

  if (*ptr == NULL || *ptr == (void *)1)
    return;

  result = htab_find_with_hash (saving_htab, *ptr, POINTER_HASH (*ptr));
  if (result == NULL)
    abort ();
  *ptr = result->new_addr;
}

/* Write out, after relocation, the pointers in TAB.  */
static void
write_pch_globals (const struct ggc_root_tab * const *tab,
		   struct traversal_state *state)
{
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  size_t i;

  for (rt = tab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	{
	  void *ptr = *(void **)((char *)rti->base + rti->stride * i);
	  struct ptr_data *new_ptr;
	  if (ptr == NULL || ptr == (void *)1)
	    {
	      if (fwrite (&ptr, sizeof (void *), 1, state->f)
		  != 1)
		fatal_error ("can't write PCH file: %m");
	    }
	  else
	    {
	      new_ptr = htab_find_with_hash (saving_htab, ptr,
					     POINTER_HASH (ptr));
	      if (fwrite (&new_ptr->new_addr, sizeof (void *), 1, state->f)
		  != 1)
		fatal_error ("can't write PCH file: %m");
	    }
	}
}

/* Hold the information we need to mmap the file back in.  */

struct mmap_info
{
  size_t offset;
  size_t size;
  void *preferred_base;
};

/* Write out the state of the compiler to F.  */

void
gt_pch_save (FILE *f)
{
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  size_t i;
  struct traversal_state state;
  char *this_object = NULL;
  size_t this_object_size = 0;
  struct mmap_info mmi;
  size_t page_size = getpagesize();

  gt_pch_save_stringpool ();

  saving_htab = htab_create (50000, saving_htab_hash, saving_htab_eq, free);

  for (rt = gt_ggc_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	(*rti->pchw)(*(void **)((char *)rti->base + rti->stride * i));

  for (rt = gt_pch_cache_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	(*rti->pchw)(*(void **)((char *)rti->base + rti->stride * i));

  /* Prepare the objects for writing, determine addresses and such.  */
  state.f = f;
  state.d = init_ggc_pch();
  state.count = 0;
  htab_traverse (saving_htab, call_count, &state);

  mmi.size = ggc_pch_total_size (state.d);

  /* Try to arrange things so that no relocation is necessary, but
     don't try very hard.  On most platforms, this will always work,
     and on the rest it's a lot of work to do better.  
     (The extra work goes in HOST_HOOKS_GT_PCH_GET_ADDRESS and
     HOST_HOOKS_GT_PCH_USE_ADDRESS.)  */
  mmi.preferred_base = host_hooks.gt_pch_get_address (mmi.size);
      
#if HAVE_MMAP_FILE
  if (mmi.preferred_base == NULL)
    {
      mmi.preferred_base = mmap (NULL, mmi.size,
				 PROT_READ | PROT_WRITE, MAP_PRIVATE,
				 fileno (state.f), 0);
      if (mmi.preferred_base == (void *) MAP_FAILED)
	mmi.preferred_base = NULL;
      else
	munmap (mmi.preferred_base, mmi.size);
    }
#endif /* HAVE_MMAP_FILE */

  ggc_pch_this_base (state.d, mmi.preferred_base);

  state.ptrs = xmalloc (state.count * sizeof (*state.ptrs));
  state.ptrs_i = 0;
  htab_traverse (saving_htab, call_alloc, &state);
  qsort (state.ptrs, state.count, sizeof (*state.ptrs), compare_ptr_data);

  /* Write out all the scalar variables.  */
  for (rt = gt_pch_scalar_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      if (fwrite (rti->base, rti->stride, 1, f) != 1)
	fatal_error ("can't write PCH file: %m");

  /* Write out all the global pointers, after translation.  */
  write_pch_globals (gt_ggc_rtab, &state);
  write_pch_globals (gt_pch_cache_rtab, &state);

  ggc_pch_prepare_write (state.d, state.f);

  /* Pad the PCH file so that the mmapped area starts on a page boundary.  */
  {
    long o;
    o = ftell (state.f) + sizeof (mmi);
    if (o == -1)
      fatal_error ("can't get position in PCH file: %m");
    mmi.offset = page_size - o % page_size;
    if (mmi.offset == page_size)
      mmi.offset = 0;
    mmi.offset += o;
  }
  if (fwrite (&mmi, sizeof (mmi), 1, state.f) != 1)
    fatal_error ("can't write PCH file: %m");
  if (mmi.offset != 0
      && fseek (state.f, mmi.offset, SEEK_SET) != 0)
    fatal_error ("can't write padding to PCH file: %m");

  /* Actually write out the objects.  */
  for (i = 0; i < state.count; i++)
    {
      if (this_object_size < state.ptrs[i]->size)
	{
	  this_object_size = state.ptrs[i]->size;
	  this_object = xrealloc (this_object, this_object_size);
	}
      memcpy (this_object, state.ptrs[i]->obj, state.ptrs[i]->size);
      if (state.ptrs[i]->reorder_fn != NULL)
	state.ptrs[i]->reorder_fn (state.ptrs[i]->obj,
				   state.ptrs[i]->note_ptr_cookie,
				   relocate_ptrs, &state);
      state.ptrs[i]->note_ptr_fn (state.ptrs[i]->obj,
				  state.ptrs[i]->note_ptr_cookie,
				  relocate_ptrs, &state);
      ggc_pch_write_object (state.d, state.f, state.ptrs[i]->obj,
			    state.ptrs[i]->new_addr, state.ptrs[i]->size, state.ptrs[i]->note_ptr_fn == gt_pch_p_S);
      if (state.ptrs[i]->note_ptr_fn != gt_pch_p_S)
	memcpy (state.ptrs[i]->obj, this_object, state.ptrs[i]->size);
    }
  ggc_pch_finish (state.d, state.f);
  gt_pch_fixup_stringpool ();

  free (state.ptrs);
  htab_delete (saving_htab);
}

/* Read the state of the compiler back in from F.  */

void
gt_pch_restore (FILE *f)
{
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  size_t i;
  struct mmap_info mmi;
  void *addr;
  bool needs_read;

  /* Delete any deletable objects.  This makes ggc_pch_read much
     faster, as it can be sure that no GCable objects remain other
     than the ones just read in.  */
  for (rt = gt_ggc_deletable_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride);

  /* Read in all the scalar variables.  */
  for (rt = gt_pch_scalar_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      if (fread (rti->base, rti->stride, 1, f) != 1)
	fatal_error ("can't read PCH file: %m");

  /* Read in all the global pointers, in 6 easy loops.  */
  for (rt = gt_ggc_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	if (fread ((char *)rti->base + rti->stride * i,
		   sizeof (void *), 1, f) != 1)
	  fatal_error ("can't read PCH file: %m");

  for (rt = gt_pch_cache_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	if (fread ((char *)rti->base + rti->stride * i,
		   sizeof (void *), 1, f) != 1)
	  fatal_error ("can't read PCH file: %m");

  if (fread (&mmi, sizeof (mmi), 1, f) != 1)
    fatal_error ("can't read PCH file: %m");

  if (host_hooks.gt_pch_use_address (mmi.preferred_base, mmi.size))
    {
#if HAVE_MMAP_FILE
      void *mmap_result;

      mmap_result = mmap (mmi.preferred_base, mmi.size,
			  PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FIXED,
			  fileno (f), mmi.offset);

      /* The file might not be mmap-able.  */
      needs_read = mmap_result == (void *) MAP_FAILED;

      /* Sanity check for broken MAP_FIXED.  */
      if (! needs_read && mmap_result != mmi.preferred_base)
	abort ();
#else
      needs_read = true;
#endif
      addr = mmi.preferred_base;
    }
  else
    {
#if HAVE_MMAP_FILE
      addr = mmap (mmi.preferred_base, mmi.size,
		   PROT_READ | PROT_WRITE, MAP_PRIVATE,
		   fileno (f), mmi.offset);
      
#if HAVE_MINCORE
      if (addr != mmi.preferred_base)
	{
	  size_t page_size = getpagesize();
	  char one_byte;
	  
	  if (addr != (void *) MAP_FAILED)
	    munmap (addr, mmi.size);
	  
	  /* We really want to be mapped at mmi.preferred_base
	     so we're going to resort to MAP_FIXED.  But before,
	     make sure that we can do so without destroying a
	     previously mapped area, by looping over all pages
	     that would be affected by the fixed mapping.  */
	  errno = 0;
	  
	  for (i = 0; i < mmi.size; i+= page_size)
	    if (mincore ((char *)mmi.preferred_base + i, page_size, 
			 (void *)&one_byte) == -1
		&& errno == ENOMEM)
	      continue; /* The page is not mapped.  */
	    else
	      break;
	  
	  if (i >= mmi.size)
	    addr = mmap (mmi.preferred_base, mmi.size, 
			 PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_FIXED,
			 fileno (f), mmi.offset);
	}
#endif /* HAVE_MINCORE */
      
      needs_read = addr == (void *) MAP_FAILED;

#else /* HAVE_MMAP_FILE */
      needs_read = true;
#endif /* HAVE_MMAP_FILE */
      if (needs_read)
	addr = xmalloc (mmi.size);
    }

  if (needs_read)
    {
      if (fseek (f, mmi.offset, SEEK_SET) != 0
	  || fread (&mmi, mmi.size, 1, f) != 1)
	fatal_error ("can't read PCH file: %m");
    }
  else if (fseek (f, mmi.offset + mmi.size, SEEK_SET) != 0)
    fatal_error ("can't read PCH file: %m");

  ggc_pch_read (f, addr);

  if (addr != mmi.preferred_base)
    {
      for (rt = gt_ggc_rtab; *rt; rt++)
	for (rti = *rt; rti->base != NULL; rti++)
	  for (i = 0; i < rti->nelt; i++)
	    {
	      char **ptr = (char **)((char *)rti->base + rti->stride * i);
	      if (*ptr != NULL)
		*ptr += (size_t)addr - (size_t)mmi.preferred_base;
	    }

      for (rt = gt_pch_cache_rtab; *rt; rt++)
	for (rti = *rt; rti->base != NULL; rti++)
	  for (i = 0; i < rti->nelt; i++)
	    {
	      char **ptr = (char **)((char *)rti->base + rti->stride * i);
	      if (*ptr != NULL)
		*ptr += (size_t)addr - (size_t)mmi.preferred_base;
	    }

      sorry ("had to relocate PCH");
    }

  gt_pch_restore_stringpool ();
}

/* Modify the bound based on rlimits.  Keep the smallest number found.  */
static double
ggc_rlimit_bound (double limit)
{
#if defined(HAVE_GETRLIMIT)
  struct rlimit rlim;
# ifdef RLIMIT_RSS
  if (getrlimit (RLIMIT_RSS, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# endif
# ifdef RLIMIT_DATA
  if (getrlimit (RLIMIT_DATA, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# endif
# ifdef RLIMIT_AS
  if (getrlimit (RLIMIT_AS, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# endif
#endif /* HAVE_GETRLIMIT */

  return limit;
}

/* Heuristic to set a default for GGC_MIN_EXPAND.  */
int
ggc_min_expand_heuristic (void)
{
  double min_expand = physmem_total();

  /* Adjust for rlimits.  */
  min_expand = ggc_rlimit_bound (min_expand);

  /* The heuristic is a percentage equal to 30% + 70%*(RAM/1GB), yielding
     a lower bound of 30% and an upper bound of 100% (when RAM >= 1GB).  */
  min_expand /= 1024*1024*1024;
  min_expand *= 70;
  min_expand = MIN (min_expand, 70);
  min_expand += 30;

  return min_expand;
}

/* Heuristic to set a default for GGC_MIN_HEAPSIZE.  */
int
ggc_min_heapsize_heuristic (void)
{
  double min_heap_kbytes = physmem_total();

  /* Adjust for rlimits.  */
  min_heap_kbytes = ggc_rlimit_bound (min_heap_kbytes);

  min_heap_kbytes /= 1024; /* Convert to Kbytes.  */

  /* The heuristic is RAM/8, with a lower bound of 4M and an upper
     bound of 128M (when RAM >= 1GB).  */
  min_heap_kbytes /= 8;
  min_heap_kbytes = MAX (min_heap_kbytes, 4 * 1024);
  min_heap_kbytes = MIN (min_heap_kbytes, 128 * 1024);

  return min_heap_kbytes;
}

void
init_ggc_heuristics (void)
{
#if !defined ENABLE_GC_CHECKING && !defined ENABLE_GC_ALWAYS_COLLECT
  set_param_value ("ggc-min-expand", ggc_min_expand_heuristic());
  set_param_value ("ggc-min-heapsize", ggc_min_heapsize_heuristic());
#endif
}
