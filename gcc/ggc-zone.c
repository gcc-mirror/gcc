/* "Bag-of-pages" zone garbage collector for the GNU compiler.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.
   Contributed by Richard Henderson (rth@redhat.com) and Daniel Berlin
   (dberlin@dberlin.org) 


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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "toplev.h"
#include "varray.h"
#include "flags.h"
#include "ggc.h"
#include "timevar.h"
#include "params.h"
#include "bitmap.h"

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
#define VALGRIND_MALLOCLIKE_BLOCK(w,x,y,z)
#define VALGRIND_FREELIKE_BLOCK(x,y)
#endif
/* Prefer MAP_ANON(YMOUS) to /dev/zero, since we don't need to keep a
   file open.  Prefer either to valloc.  */
#ifdef HAVE_MMAP_ANON
# undef HAVE_MMAP_DEV_ZERO

# include <sys/mman.h>
# ifndef MAP_FAILED
#  define MAP_FAILED -1
# endif
# if !defined (MAP_ANONYMOUS) && defined (MAP_ANON)
#  define MAP_ANONYMOUS MAP_ANON
# endif
# define USING_MMAP

#endif

#ifdef HAVE_MMAP_DEV_ZERO

# include <sys/mman.h>
# ifndef MAP_FAILED
#  define MAP_FAILED -1
# endif
# define USING_MMAP

#endif

#ifndef USING_MMAP
#error "Zone collector requires mmap"
#endif

#if (GCC_VERSION < 3001)
#define prefetch(X) ((void) X)
#else
#define prefetch(X) __builtin_prefetch (X)
#endif

/* NOTES:
   If we track inter-zone pointers, we can mark single zones at a
   time.
   If we have a zone where we guarantee no inter-zone pointers, we
   could mark that zone separately.
   The garbage zone should not be marked, and we should return 1 in
   ggc_set_mark for any object in the garbage zone, which cuts off
   marking quickly.  */
/* Stategy:

   This garbage-collecting allocator segregates objects into zones.
   It also segregates objects into "large" and "small" bins.  Large
   objects are greater or equal to page size.

   Pages for small objects are broken up into chunks, each of which
   are described by a struct alloc_chunk.  One can walk over all
   chunks on the page by adding the chunk size to the chunk's data
   address.  The free space for a page exists in the free chunk bins.

   Each page-entry also has a context depth, which is used to track
   pushing and popping of allocation contexts.  Only objects allocated
   in the current (highest-numbered) context may be collected.

   Empty pages (of all sizes) are kept on a single page cache list,
   and are considered first when new pages are required; they are
   deallocated at the start of the next collection if they haven't
   been recycled by then.  */

/* Define GGC_DEBUG_LEVEL to print debugging information.
     0: No debugging output.
     1: GC statistics only.
     2: Page-entry allocations/deallocations as well.
     3: Object allocations as well.
     4: Object marks as well.  */
#define GGC_DEBUG_LEVEL (0)

#ifndef HOST_BITS_PER_PTR
#define HOST_BITS_PER_PTR  HOST_BITS_PER_LONG
#endif
#ifdef COOKIE_CHECKING
#define CHUNK_MAGIC 0x95321123
#define DEADCHUNK_MAGIC 0x12817317
#endif

/* This structure manages small chunks.  When the chunk is free, it's
   linked with other chunks via free_next.  When the chunk is allocated,
   the data starts at u.  Large chunks are allocated one at a time to
   their own page, and so don't come in here.

   The "type" field is a placeholder for a future change to do
   generational collection.  At present it is 0 when free and
   and 1 when allocated.  */

struct alloc_chunk {
#ifdef COOKIE_CHECKING
  unsigned int magic;
#endif
  unsigned int type:1;
  unsigned int typecode:14;
  unsigned int large:1;
  unsigned int size:15;
  unsigned int mark:1;
  union {
    struct alloc_chunk *next_free;
    char data[1];

    /* Make sure the data is sufficiently aligned.  */
    HOST_WIDEST_INT align_i;
#ifdef HAVE_LONG_DOUBLE
    long double align_d;
#else
    double align_d;
#endif
  } u;
} __attribute__ ((packed));

#define CHUNK_OVERHEAD	(offsetof (struct alloc_chunk, u))

/* We maintain several bins of free lists for chunks for very small
   objects.  We never exhaustively search other bins -- if we don't
   find one of the proper size, we allocate from the "larger" bin.  */

/* Decreasing the number of free bins increases the time it takes to allocate.
   Similar with increasing max_free_bin_size without increasing num_free_bins.

   After much histogramming of allocation sizes and time spent on gc,
   on a PowerPC G4 7450 - 667 mhz, and a Pentium 4 - 2.8ghz,
   these were determined to be the optimal values.  */
#define NUM_FREE_BINS		64
#define MAX_FREE_BIN_SIZE	256
#define FREE_BIN_DELTA		(MAX_FREE_BIN_SIZE / NUM_FREE_BINS)
#define SIZE_BIN_UP(SIZE)	(((SIZE) + FREE_BIN_DELTA - 1) / FREE_BIN_DELTA)
#define SIZE_BIN_DOWN(SIZE)	((SIZE) / FREE_BIN_DELTA)

/* Marker used as chunk->size for a large object.  Should correspond
   to the size of the bitfield above.  */
#define LARGE_OBJECT_SIZE	0x7fff

/* We use this structure to determine the alignment required for
   allocations.  For power-of-two sized allocations, that's not a
   problem, but it does matter for odd-sized allocations.  */

struct max_alignment {
  char c;
  union {
    HOST_WIDEST_INT i;
#ifdef HAVE_LONG_DOUBLE
    long double d;
#else
    double d;
#endif
  } u;
};

/* The biggest alignment required.  */

#define MAX_ALIGNMENT (offsetof (struct max_alignment, u))

/* Compute the smallest nonnegative number which when added to X gives
   a multiple of F.  */

#define ROUND_UP_VALUE(x, f) ((f) - 1 - ((f) - 1 + (x)) % (f))

/* Compute the smallest multiple of F that is >= X.  */

#define ROUND_UP(x, f) (CEIL (x, f) * (f))


/* A page_entry records the status of an allocation page.  */
typedef struct page_entry
{
  /* The next page-entry with objects of the same size, or NULL if
     this is the last page-entry.  */
  struct page_entry *next;

  /* The number of bytes allocated.  (This will always be a multiple
     of the host system page size.)  */
  size_t bytes;

  /* How many collections we've survived.  */
  size_t survived;

  /* The address at which the memory is allocated.  */
  char *page;

  /* Context depth of this page.  */
  unsigned short context_depth;

  /* Does this page contain small objects, or one large object?  */
  bool large_p;

  /* The zone that this page entry belongs to.  */
  struct alloc_zone *zone;
} page_entry;


/* The global variables.  */
static struct globals
{
  /* The linked list of zones.  */
  struct alloc_zone *zones;

  /* The system's page size.  */
  size_t pagesize;
  size_t lg_pagesize;

  /* A file descriptor open to /dev/zero for reading.  */
#if defined (HAVE_MMAP_DEV_ZERO)
  int dev_zero_fd;
#endif

  /* The file descriptor for debugging output.  */
  FILE *debug_file;
} G;

/*  The zone allocation structure.  */
struct alloc_zone
{
  /* Name of the zone.  */
  const char *name;

  /* Linked list of pages in a zone.  */
  page_entry *pages;

  /* Linked lists of free storage.  Slots 1 ... NUM_FREE_BINS have chunks of size
     FREE_BIN_DELTA.  All other chunks are in slot 0.  */
  struct alloc_chunk *free_chunks[NUM_FREE_BINS + 1];

  /* Bytes currently allocated.  */
  size_t allocated;

  /* Bytes currently allocated at the end of the last collection.  */
  size_t allocated_last_gc;

  /* Total amount of memory mapped.  */
  size_t bytes_mapped;

  /* Bit N set if any allocations have been done at context depth N.  */
  unsigned long context_depth_allocations;

  /* Bit N set if any collections have been done at context depth N.  */
  unsigned long context_depth_collections;

  /* The current depth in the context stack.  */
  unsigned short context_depth;

  /* A cache of free system pages.  */
  page_entry *free_pages;

  /* Next zone in the linked list of zones.  */
  struct alloc_zone *next_zone;

  /* True if this zone was collected during this collection.  */
  bool was_collected;

  /* True if this zone should be destroyed after the next collection.  */
  bool dead;
} main_zone;

struct alloc_zone *rtl_zone;
struct alloc_zone *garbage_zone;
struct alloc_zone *tree_zone;

/* Allocate pages in chunks of this size, to throttle calls to memory
   allocation routines.  The first page is used, the rest go onto the
   free list.  This cannot be larger than HOST_BITS_PER_INT for the
   in_use bitmask for page_group.  */
#define GGC_QUIRE_SIZE 16

static int ggc_allocated_p (const void *);
#ifdef USING_MMAP
static char *alloc_anon (char *, size_t, struct alloc_zone *);
#endif
static struct page_entry * alloc_small_page ( struct alloc_zone *);
static struct page_entry * alloc_large_page (size_t, struct alloc_zone *);
static void free_chunk (struct alloc_chunk *, size_t, struct alloc_zone *);
static void free_page (struct page_entry *);
static void release_pages (struct alloc_zone *);
static void sweep_pages (struct alloc_zone *);
static void * ggc_alloc_zone_1 (size_t, struct alloc_zone *, short);
static bool ggc_collect_1 (struct alloc_zone *, bool);
static void check_cookies (void);


/* Returns nonzero if P was allocated in GC'able memory.  */

static inline int
ggc_allocated_p (const void *p)
{
  struct alloc_chunk *chunk;
  chunk = (struct alloc_chunk *) ((char *)p - CHUNK_OVERHEAD);
#ifdef COOKIE_CHECKING
  if (chunk->magic != CHUNK_MAGIC)
    abort ();
#endif
  if (chunk->type == 1)
    return true;  
  return false;
}


#ifdef USING_MMAP
/* Allocate SIZE bytes of anonymous memory, preferably near PREF,
   (if non-null).  The ifdef structure here is intended to cause a
   compile error unless exactly one of the HAVE_* is defined.  */

static inline char *
alloc_anon (char *pref ATTRIBUTE_UNUSED, size_t size, struct alloc_zone *zone)
{
#ifdef HAVE_MMAP_ANON
  char *page = (char *) mmap (pref, size, PROT_READ | PROT_WRITE,
			      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#endif
#ifdef HAVE_MMAP_DEV_ZERO
  char *page = (char *) mmap (pref, size, PROT_READ | PROT_WRITE,
			      MAP_PRIVATE, G.dev_zero_fd, 0);
#endif
  VALGRIND_MALLOCLIKE_BLOCK(page, size, 0, 0);

  if (page == (char *) MAP_FAILED)
    {
      perror ("virtual memory exhausted");
      exit (FATAL_EXIT_CODE);
    }

  /* Remember that we allocated this memory.  */
  zone->bytes_mapped += size;
  /* Pretend we don't have access to the allocated pages.  We'll enable
     access to smaller pieces of the area in ggc_alloc.  Discard the
     handle to avoid handle leak.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_NOACCESS (page, size));
  return page;
}
#endif

/* Allocate a new page for allocating objects of size 2^ORDER,
   and return an entry for it.  */

static inline struct page_entry *
alloc_small_page (struct alloc_zone *zone)
{
  struct page_entry *entry;
  char *page;

  page = NULL;

  /* Check the list of free pages for one we can use.  */
  entry = zone->free_pages;
  if (entry != NULL)
    {
      /* Recycle the allocated memory from this page ...  */
      zone->free_pages = entry->next;
      page = entry->page;


    }
#ifdef USING_MMAP
  else
    {
      /* We want just one page.  Allocate a bunch of them and put the
	 extras on the freelist.  (Can only do this optimization with
	 mmap for backing store.)  */
      struct page_entry *e, *f = zone->free_pages;
      int i;

      page = alloc_anon (NULL, G.pagesize * GGC_QUIRE_SIZE, zone);

      /* This loop counts down so that the chain will be in ascending
	 memory order.  */
      for (i = GGC_QUIRE_SIZE - 1; i >= 1; i--)
	{
	  e = (struct page_entry *) xmalloc (sizeof (struct page_entry));
	  e->bytes = G.pagesize;
	  e->page = page + (i << G.lg_pagesize);
	  e->next = f;
	  f = e;
	}

      zone->free_pages = f;
    }
#endif
  if (entry == NULL)
    entry = (struct page_entry *) xmalloc (sizeof (struct page_entry));

  entry->next = 0;
  entry->bytes = G.pagesize;
  entry->page = page;
  entry->context_depth = zone->context_depth;
  entry->large_p = false;
  entry->zone = zone;
  zone->context_depth_allocations |= (unsigned long)1 << zone->context_depth;

  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file,
	     "Allocating %s page at %p, data %p-%p\n", entry->zone->name,
	     (PTR) entry, page, page + G.pagesize - 1);

  return entry;
}
/* Compute the smallest multiple of F that is >= X.  */

#define ROUND_UP(x, f) (CEIL (x, f) * (f))

/* Allocate a large page of size SIZE in ZONE.  */

static inline struct page_entry *
alloc_large_page (size_t size, struct alloc_zone *zone)
{
  struct page_entry *entry;
  char *page;
  size =  ROUND_UP (size, 1024);
  page = (char *) xmalloc (size + CHUNK_OVERHEAD + sizeof (struct page_entry));
  entry = (struct page_entry *) (page + size + CHUNK_OVERHEAD);

  entry->next = 0;
  entry->bytes = size;
  entry->page = page;
  entry->context_depth = zone->context_depth;
  entry->large_p = true;
  entry->zone = zone;
  zone->context_depth_allocations |= (unsigned long)1 << zone->context_depth;

  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file,
	     "Allocating %s large page at %p, data %p-%p\n", entry->zone->name,
	     (PTR) entry, page, page + size - 1);

  return entry;
}


/* For a page that is no longer needed, put it on the free page list.  */

static inline void
free_page (page_entry *entry)
{
  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file,
	     "Deallocating %s page at %p, data %p-%p\n", entry->zone->name, (PTR) entry,
	     entry->page, entry->page + entry->bytes - 1);

  if (entry->large_p)
    {
      free (entry->page);
      VALGRIND_FREELIKE_BLOCK (entry->page, entry->bytes);
    }
  else
    {
      /* Mark the page as inaccessible.  Discard the handle to
	 avoid handle leak.  */
      VALGRIND_DISCARD (VALGRIND_MAKE_NOACCESS (entry->page, entry->bytes));

      entry->next = entry->zone->free_pages;
      entry->zone->free_pages = entry;
    }
}

/* Release the free page cache to the system.  */

static void
release_pages (struct alloc_zone *zone)
{
#ifdef USING_MMAP
  page_entry *p, *next;
  char *start;
  size_t len;

  /* Gather up adjacent pages so they are unmapped together.  */
  p = zone->free_pages;

  while (p)
    {
      start = p->page;
      next = p->next;
      len = p->bytes;
      free (p);
      p = next;

      while (p && p->page == start + len)
	{
	  next = p->next;
	  len += p->bytes;
	  free (p);
	  p = next;
	}

      munmap (start, len);
      zone->bytes_mapped -= len;
    }

  zone->free_pages = NULL;
#endif
}

/* Place CHUNK of size SIZE on the free list for ZONE.  */

static inline void
free_chunk (struct alloc_chunk *chunk, size_t size, struct alloc_zone *zone)
{
  size_t bin = 0;

  bin = SIZE_BIN_DOWN (size);
  if (bin == 0)
    abort ();
  if (bin > NUM_FREE_BINS)
    bin = 0;
#ifdef COOKIE_CHECKING
  if (chunk->magic != CHUNK_MAGIC && chunk->magic != DEADCHUNK_MAGIC)
    abort ();
  chunk->magic = DEADCHUNK_MAGIC;
#endif
  chunk->u.next_free = zone->free_chunks[bin];
  zone->free_chunks[bin] = chunk;
  if (GGC_DEBUG_LEVEL >= 3)
    fprintf (G.debug_file, "Deallocating object, chunk=%p\n", (void *)chunk);
  VALGRIND_DISCARD (VALGRIND_MAKE_READABLE (chunk, sizeof (struct alloc_chunk)));
}

/* Allocate a chunk of memory of SIZE bytes.  */

static void *
ggc_alloc_zone_1 (size_t size, struct alloc_zone *zone, short type)
{
  size_t bin = 0;
  size_t lsize = 0;
  struct page_entry *entry;
  struct alloc_chunk *chunk, *lchunk, **pp;
  void *result;

  /* Align size, so that we're assured of aligned allocations.  */
  if (size < FREE_BIN_DELTA)
    size = FREE_BIN_DELTA;
  size = (size + MAX_ALIGNMENT - 1) & -MAX_ALIGNMENT;

  /* Large objects are handled specially.  */
  if (size >= G.pagesize - 2*CHUNK_OVERHEAD - FREE_BIN_DELTA)
    {
      size = ROUND_UP (size, 1024);
      entry = alloc_large_page (size, zone);
      entry->survived = 0;
      entry->next = entry->zone->pages;
      entry->zone->pages = entry;

      chunk = (struct alloc_chunk *) entry->page;
      VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (chunk, sizeof (struct alloc_chunk)));
      chunk->large = 1;
      chunk->size = CEIL (size, 1024);

      goto found;
    }

  /* First look for a tiny object already segregated into its own
     size bucket.  */
  bin = SIZE_BIN_UP (size);
  if (bin <= NUM_FREE_BINS)
    {
      chunk = zone->free_chunks[bin];
      if (chunk)
	{
	  zone->free_chunks[bin] = chunk->u.next_free;
	  VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (chunk, sizeof (struct alloc_chunk)));
	  goto found;
	}
    }

  /* Failing that, look through the "other" bucket for a chunk
     that is large enough.  */
  pp = &(zone->free_chunks[0]);
  chunk = *pp;
  while (chunk && chunk->size < size)
    {
      pp = &chunk->u.next_free;
      chunk = *pp;
    }

  /* Failing that, allocate new storage.  */
  if (!chunk)
    {
      entry = alloc_small_page (zone);
      entry->next = entry->zone->pages;
      entry->zone->pages = entry;

      chunk = (struct alloc_chunk *) entry->page;
      VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (chunk, sizeof (struct alloc_chunk)));
      chunk->size = G.pagesize - CHUNK_OVERHEAD;
      chunk->large = 0;
    }
  else
    {
      *pp = chunk->u.next_free;
      VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (chunk, sizeof (struct alloc_chunk)));
      chunk->large = 0;
    }
  /* Release extra memory from a chunk that's too big.  */
  lsize = chunk->size - size;
  if (lsize >= CHUNK_OVERHEAD + FREE_BIN_DELTA)
    {
      VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (chunk, sizeof (struct alloc_chunk)));
      chunk->size = size;

      lsize -= CHUNK_OVERHEAD;
      lchunk = (struct alloc_chunk *)(chunk->u.data + size);
      VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (lchunk, sizeof (struct alloc_chunk)));
#ifdef COOKIE_CHECKING
      lchunk->magic = CHUNK_MAGIC;
#endif
      lchunk->type = 0;
      lchunk->mark = 0;
      lchunk->size = lsize;
      lchunk->large = 0;
      free_chunk (lchunk, lsize, zone);
    }
  /* Calculate the object's address.  */
 found:
#ifdef COOKIE_CHECKING
  chunk->magic = CHUNK_MAGIC;
#endif
  chunk->type = 1;
  chunk->mark = 0;
  chunk->typecode = type;
  result = chunk->u.data;

#ifdef ENABLE_GC_CHECKING
  /* Keep poisoning-by-writing-0xaf the object, in an attempt to keep the
     exact same semantics in presence of memory bugs, regardless of
     ENABLE_VALGRIND_CHECKING.  We override this request below.  Drop the
     handle to avoid handle leak.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (result, size));

  /* `Poison' the entire allocated object.  */
  memset (result, 0xaf, size);
#endif

  /* Tell Valgrind that the memory is there, but its content isn't
     defined.  The bytes at the end of the object are still marked
     unaccessible.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_WRITABLE (result, size));

  /* Keep track of how many bytes are being allocated.  This
     information is used in deciding when to collect.  */
  zone->allocated += size + CHUNK_OVERHEAD;

  if (GGC_DEBUG_LEVEL >= 3)
    fprintf (G.debug_file, "Allocating object, chunk=%p size=%lu at %p\n",
	     (void *)chunk, (unsigned long) size, result);

  return result;
}

/* Allocate a SIZE of chunk memory of GTE type, into an appropriate zone
   for that type.  */

void *
ggc_alloc_typed (enum gt_types_enum gte, size_t size)
{
  switch (gte)
    {
    case gt_ggc_e_14lang_tree_node:
      return ggc_alloc_zone_1 (size, tree_zone, gte);

    case gt_ggc_e_7rtx_def:
      return ggc_alloc_zone_1 (size, rtl_zone, gte);

    case gt_ggc_e_9rtvec_def:
      return ggc_alloc_zone_1 (size, rtl_zone, gte);

    default:
      return ggc_alloc_zone_1 (size, &main_zone, gte);
    }
}

/* Normal ggc_alloc simply allocates into the main zone.  */

void *
ggc_alloc (size_t size)
{
  return ggc_alloc_zone_1 (size, &main_zone, -1);
}

/* Zone allocation allocates into the specified zone.  */

void *
ggc_alloc_zone (size_t size, struct alloc_zone *zone)
{
  return ggc_alloc_zone_1 (size, zone, -1);
}

/* If P is not marked, mark it and return false.  Otherwise return true.
   P must have been allocated by the GC allocator; it mustn't point to
   static objects, stack variables, or memory allocated with malloc.  */

int
ggc_set_mark (const void *p)
{
  struct alloc_chunk *chunk;

  chunk = (struct alloc_chunk *) ((char *)p - CHUNK_OVERHEAD);
#ifdef COOKIE_CHECKING
  if (chunk->magic != CHUNK_MAGIC)
    abort ();
#endif
  if (chunk->mark)
    return 1;
  chunk->mark = 1;

  if (GGC_DEBUG_LEVEL >= 4)
    fprintf (G.debug_file, "Marking %p\n", p);

  return 0;
}

/* Return 1 if P has been marked, zero otherwise.
   P must have been allocated by the GC allocator; it mustn't point to
   static objects, stack variables, or memory allocated with malloc.  */

int
ggc_marked_p (const void *p)
{
  struct alloc_chunk *chunk;

  chunk = (struct alloc_chunk *) ((char *)p - CHUNK_OVERHEAD);
#ifdef COOKIE_CHECKING
  if (chunk->magic != CHUNK_MAGIC)
    abort ();
#endif
  return chunk->mark;
}

/* Return the size of the gc-able object P.  */

size_t
ggc_get_size (const void *p)
{
  struct alloc_chunk *chunk;

  chunk = (struct alloc_chunk *) ((char *)p - CHUNK_OVERHEAD);
#ifdef COOKIE_CHECKING
  if (chunk->magic != CHUNK_MAGIC)
    abort ();
#endif
  if (chunk->large)
    return chunk->size * 1024;

  return chunk->size;
}

/* Initialize the ggc-zone-mmap allocator.  */
void
init_ggc (void)
{
  /* Set up the main zone by hand.  */
  main_zone.name = "Main zone";
  G.zones = &main_zone;

  /* Allocate the default zones.  */
  rtl_zone = new_ggc_zone ("RTL zone");
  tree_zone = new_ggc_zone ("Tree zone");
  garbage_zone = new_ggc_zone ("Garbage zone");

  G.pagesize = getpagesize();
  G.lg_pagesize = exact_log2 (G.pagesize);
#ifdef HAVE_MMAP_DEV_ZERO
  G.dev_zero_fd = open ("/dev/zero", O_RDONLY);
  if (G.dev_zero_fd == -1)
    abort ();
#endif

#if 0
  G.debug_file = fopen ("ggc-mmap.debug", "w");
  setlinebuf (G.debug_file);
#else
  G.debug_file = stdout;
#endif

#ifdef USING_MMAP
  /* StunOS has an amazing off-by-one error for the first mmap allocation
     after fiddling with RLIMIT_STACK.  The result, as hard as it is to
     believe, is an unaligned page allocation, which would cause us to
     hork badly if we tried to use it.  */
  {
    char *p = alloc_anon (NULL, G.pagesize, &main_zone);
    struct page_entry *e;
    if ((size_t)p & (G.pagesize - 1))
      {
	/* How losing.  Discard this one and try another.  If we still
	   can't get something useful, give up.  */

	p = alloc_anon (NULL, G.pagesize, &main_zone);
	if ((size_t)p & (G.pagesize - 1))
	  abort ();
      }

    /* We have a good page, might as well hold onto it...  */
    e = (struct page_entry *) xmalloc (sizeof (struct page_entry));
    e->bytes = G.pagesize;
    e->page = p;
    e->next = main_zone.free_pages;
    main_zone.free_pages = e;
  }
#endif
}

/* Start a new GGC zone.  */

struct alloc_zone *
new_ggc_zone (const char * name)
{
  struct alloc_zone *new_zone = xcalloc (1, sizeof (struct alloc_zone));
  new_zone->name = name;
  new_zone->next_zone = G.zones->next_zone;
  G.zones->next_zone = new_zone;
  return new_zone;
}

/* Destroy a GGC zone.  */
void
destroy_ggc_zone (struct alloc_zone * dead_zone)
{
  struct alloc_zone *z;

  for (z = G.zones; z && z->next_zone != dead_zone; z = z->next_zone)
    /* Just find that zone.  */ ;

#ifdef ENABLE_CHECKING
  /* We should have found the zone in the list.  Anything else is fatal.  */
  if (!z)
    abort ();
#endif

  /* z is dead, baby. z is dead.  */
  z->dead= true;
}

/* Increment the `GC context'.  Objects allocated in an outer context
   are never freed, eliminating the need to register their roots.  */

void
ggc_push_context (void)
{
  struct alloc_zone *zone;
  for (zone = G.zones; zone; zone = zone->next_zone)
    ++(zone->context_depth);
  /* Die on wrap.  */
  if (main_zone.context_depth >= HOST_BITS_PER_LONG)
    abort ();
}

/* Decrement the `GC context'.  All objects allocated since the
   previous ggc_push_context are migrated to the outer context.  */

static void
ggc_pop_context_1 (struct alloc_zone *zone)
{
  unsigned long omask;
  unsigned depth;
  page_entry *p;

  depth = --(zone->context_depth);
  omask = (unsigned long)1 << (depth + 1);

  if (!((zone->context_depth_allocations | zone->context_depth_collections) & omask))
    return;

  zone->context_depth_allocations |= (zone->context_depth_allocations & omask) >> 1;
  zone->context_depth_allocations &= omask - 1;
  zone->context_depth_collections &= omask - 1;

  /* Any remaining pages in the popped context are lowered to the new
     current context; i.e. objects allocated in the popped context and
     left over are imported into the previous context.  */
  for (p = zone->pages; p != NULL; p = p->next)
    if (p->context_depth > depth)
      p->context_depth = depth;
}

/* Pop all the zone contexts.  */

void
ggc_pop_context (void)
{
  struct alloc_zone *zone;
  for (zone = G.zones; zone; zone = zone->next_zone)
    ggc_pop_context_1 (zone);
}
/* Poison the chunk.  */
#ifdef ENABLE_GC_CHECKING
#define poison_chunk(CHUNK, SIZE) \
  memset ((CHUNK)->u.data, 0xa5, (SIZE))
#else
#define poison_chunk(CHUNK, SIZE)
#endif

/* Free all empty pages and objects within a page for a given zone  */

static void
sweep_pages (struct alloc_zone *zone)
{
  page_entry **pp, *p, *next;
  struct alloc_chunk *chunk, *last_free, *end;
  size_t last_free_size, allocated = 0;
  bool nomarksinpage;
  /* First, reset the free_chunks lists, since we are going to
     re-free free chunks in hopes of coalescing them into large chunks.  */
  memset (zone->free_chunks, 0, sizeof (zone->free_chunks));
  pp = &zone->pages;
  for (p = zone->pages; p ; p = next)
    {
      next = p->next;
      /* Large pages are all or none affairs. Either they are
	 completely empty, or they are completely full.
	 
	 XXX: Should we bother to increment allocated.  */
      if (p->large_p)
	{
	  if (((struct alloc_chunk *)p->page)->mark == 1)
	    {
	      ((struct alloc_chunk *)p->page)->mark = 0;
	    }
	  else
	    {
	      *pp = next;
#ifdef ENABLE_GC_CHECKING
	  /* Poison the page.  */
	  memset (p->page, 0xb5, p->bytes);
#endif
	      free_page (p);
	    }
	  continue;
	}

      /* This page has now survived another collection.  */
      p->survived++;

      /* Which leaves full and partial pages.  Step through all chunks,
	 consolidate those that are free and insert them into the free
	 lists.  Note that consolidation slows down collection
	 slightly.  */

      chunk = (struct alloc_chunk *)p->page;
      end = (struct alloc_chunk *)(p->page + G.pagesize);
      last_free = NULL;
      last_free_size = 0;
      nomarksinpage = true;
      do
	{
	  prefetch ((struct alloc_chunk *)(chunk->u.data + chunk->size));
	  if (chunk->mark || p->context_depth < zone->context_depth)
	    {
	      nomarksinpage = false;
	      if (last_free)
		{
		  last_free->type = 0;
		  last_free->size = last_free_size;
		  last_free->mark = 0;
		  poison_chunk (last_free, last_free_size);
		  free_chunk (last_free, last_free_size, zone);
		  last_free = NULL;
		}
	      if (chunk->mark)
	        {
	          allocated += chunk->size + CHUNK_OVERHEAD;
		}
	      chunk->mark = 0;
	    }
	  else
	    {
	      if (last_free)
	        {
		  last_free_size += CHUNK_OVERHEAD + chunk->size;
		}
	      else
		{
		  last_free = chunk;
		  last_free_size = chunk->size;
		}
	    }

	  chunk = (struct alloc_chunk *)(chunk->u.data + chunk->size);
	}
      while (chunk < end);

      if (nomarksinpage)
	{
	  *pp = next;
#ifdef ENABLE_GC_CHECKING
	  /* Poison the page.  */
	  memset (p->page, 0xb5, p->bytes);
#endif
	  free_page (p);
	  continue;
	}
      else if (last_free)
	{
	  last_free->type = 0;
	  last_free->size = last_free_size;
	  last_free->mark = 0;
	  poison_chunk (last_free, last_free_size);
	  free_chunk (last_free, last_free_size, zone);
	}
      pp = &p->next;
    }

  zone->allocated = allocated;
}

/* mark-and-sweep routine for collecting a single zone.  NEED_MARKING
   is true if we need to mark before sweeping, false if some other
   zone collection has already performed marking for us.  Returns true
   if we collected, false otherwise.  */

static bool
ggc_collect_1 (struct alloc_zone *zone, bool need_marking)
{
  if (!zone->dead)
    {
      /* Avoid frequent unnecessary work by skipping collection if the
	 total allocations haven't expanded much since the last
	 collection.  */
      float allocated_last_gc =
	MAX (zone->allocated_last_gc,
	     (size_t) PARAM_VALUE (GGC_MIN_HEAPSIZE) * 1024);

      float min_expand = allocated_last_gc * PARAM_VALUE (GGC_MIN_EXPAND) / 100;

      if (zone->allocated < allocated_last_gc + min_expand)
	return false;
    }

  if (!quiet_flag)
    fprintf (stderr, " {%s GC %luk -> ",
	     zone->name, (unsigned long) zone->allocated / 1024);

  /* Zero the total allocated bytes.  This will be recalculated in the
     sweep phase.  */
  zone->allocated = 0;

  /* Release the pages we freed the last time we collected, but didn't
     reuse in the interim.  */
  release_pages (zone);

  /* Indicate that we've seen collections at this context depth.  */
  zone->context_depth_collections
    = ((unsigned long)1 << (zone->context_depth + 1)) - 1;
  if (need_marking)
    ggc_mark_roots ();
  sweep_pages (zone);
  zone->was_collected = true;
  zone->allocated_last_gc = zone->allocated;

  if (!quiet_flag)
    fprintf (stderr, "%luk}", (unsigned long) zone->allocated / 1024);
  return true;
}

/* Calculate the average page survival rate in terms of number of
   collections.  */

static float
calculate_average_page_survival (struct alloc_zone *zone)
{
  float count = 0.0;
  float survival = 0.0;
  page_entry *p;
  for (p = zone->pages; p; p = p->next)
    {
      count += 1.0;
      survival += p->survived;
    }
  return survival/count;
}

/* Check the magic cookies all of the chunks contain, to make sure we
   aren't doing anything stupid, like stomping on alloc_chunk
   structures.  */

static inline void
check_cookies (void)
{
#ifdef COOKIE_CHECKING
  page_entry *p;
  struct alloc_zone *zone;

  for (zone = G.zones; zone; zone = zone->next_zone)
    {
      for (p = zone->pages; p; p = p->next)
	{
	  if (!p->large_p)
	    {
	      struct alloc_chunk *chunk = (struct alloc_chunk *)p->page;
	      struct alloc_chunk *end = (struct alloc_chunk *)(p->page + G.pagesize);
	      do
		{
		  if (chunk->magic != CHUNK_MAGIC && chunk->magic != DEADCHUNK_MAGIC)
		    abort ();
		  chunk = (struct alloc_chunk *)(chunk->u.data + chunk->size);
		}
	      while (chunk < end);
	    }
	}
    }
#endif
}
/* Top level collection routine.  */

void
ggc_collect (void)
{
  struct alloc_zone *zone;
  bool marked = false;
  float f;

  timevar_push (TV_GC);
  check_cookies ();
  /* Start by possibly collecting the main zone.  */
  main_zone.was_collected = false;
  marked |= ggc_collect_1 (&main_zone, true);

  /* In order to keep the number of collections down, we don't
     collect other zones unless we are collecting the main zone.  This
     gives us roughly the same number of collections as we used to
     have with the old gc.  The number of collection is important
     because our main slowdown (according to profiling) is now in
     marking.  So if we mark twice as often as we used to, we'll be
     twice as slow.  Hopefully we'll avoid this cost when we mark
     zone-at-a-time.  */

  if (main_zone.was_collected)
    {
      struct alloc_zone *zone;

      for (zone = main_zone.next_zone; zone; zone = zone->next_zone)
	{
	  check_cookies ();
	  zone->was_collected = false;
	  marked |= ggc_collect_1 (zone, !marked);
	}
    }

  /* Print page survival stats, if someone wants them.  */
  if (GGC_DEBUG_LEVEL >= 2)
    {
      for (zone = G.zones; zone; zone = zone->next_zone)
	{
	  if (zone->was_collected)
	    {
	      f = calculate_average_page_survival (zone);
	      printf ("Average page survival in zone `%s' is %f\n",
		      zone->name, f);
	    }
	}
    }

  /* Since we don't mark zone at a time right now, marking in any
     zone means marking in every zone. So we have to clear all the
     marks in all the zones that weren't collected already.  */
  if (marked)
    {
      page_entry *p;
      for (zone = G.zones; zone; zone = zone->next_zone)
      {
	if (zone->was_collected)
	  continue;
	for (p = zone->pages; p; p = p->next)
	  {
	    if (!p->large_p)
	      {
		struct alloc_chunk *chunk = (struct alloc_chunk *)p->page;
		struct alloc_chunk *end = (struct alloc_chunk *)(p->page + G.pagesize);
		do
		  {
		    prefetch ((struct alloc_chunk *)(chunk->u.data + chunk->size));
		    if (chunk->mark || p->context_depth < zone->context_depth)
		      {
			chunk->mark = 0;
		      }
		    chunk = (struct alloc_chunk *)(chunk->u.data + chunk->size);
		  }
		while (chunk < end);
	      }
	    else
	      {
		((struct alloc_chunk *)p->page)->mark = 0;
	      }
	  }
      }
    }

  /* Free dead zones.  */
  for (zone = G.zones; zone && zone->next_zone; zone = zone->next_zone)
    {
      if (zone->next_zone->dead)
	{
	  struct alloc_zone *dead_zone = zone->next_zone;

	  printf ("Zone `%s' is dead and will be freed.\n", dead_zone->name);

	  /* The zone must be empty.  */
	  if (dead_zone->allocated != 0)
	    abort ();

	  /* Unchain the dead zone, release all its pages and free it.  */
	  zone->next_zone = zone->next_zone->next_zone;
	  release_pages (dead_zone);
	  free (dead_zone);
	}
    }

  timevar_pop (TV_GC);
}

/* Print allocation statistics.  */

void
ggc_print_statistics (void)
{
}

struct ggc_pch_data
{
  struct ggc_pch_ondisk
  {
    unsigned total;
  } d;
  size_t base;
  size_t written;
};

/* Initialize the PCH datastructure.  */

struct ggc_pch_data *
init_ggc_pch (void)
{
  return xcalloc (sizeof (struct ggc_pch_data), 1);
}

/* Add the size of object X to the size of the PCH data.  */

void
ggc_pch_count_object (struct ggc_pch_data *d, void *x ATTRIBUTE_UNUSED,
		      size_t size, bool is_string)
{
  if (!is_string)
    {
      d->d.total += size + CHUNK_OVERHEAD;
    }
  else
    d->d.total += size;
}

/* Return the total size of the PCH data.  */

size_t
ggc_pch_total_size (struct ggc_pch_data *d)
{
  return d->d.total;
}

/* Set the base address for the objects in the PCH file.  */

void
ggc_pch_this_base (struct ggc_pch_data *d, void *base)
{
  d->base = (size_t) base;
}

/* Allocate a place for object X of size SIZE in the PCH file.  */

char *
ggc_pch_alloc_object (struct ggc_pch_data *d, void *x,
		      size_t size, bool is_string)
{
  char *result;
  result = (char *)d->base;
  if (!is_string)
    {
      struct alloc_chunk *chunk = (struct alloc_chunk *) ((char *)x - CHUNK_OVERHEAD);
      if (chunk->large)
	d->base += ggc_get_size (x) + CHUNK_OVERHEAD;
      else
	d->base += chunk->size + CHUNK_OVERHEAD;
      return result + CHUNK_OVERHEAD;
    }
  else
    {
      d->base += size;
      return result;
    }

}

/* Prepare to write out the PCH data to file F.  */

void
ggc_pch_prepare_write (struct ggc_pch_data *d ATTRIBUTE_UNUSED,
		       FILE *f ATTRIBUTE_UNUSED)
{
  /* Nothing to do.  */
}

/* Write out object X of SIZE to file F.  */

void
ggc_pch_write_object (struct ggc_pch_data *d ATTRIBUTE_UNUSED,
		      FILE *f, void *x, void *newx ATTRIBUTE_UNUSED,
		      size_t size, bool is_string)
{
  if (!is_string)
    {
      struct alloc_chunk *chunk = (struct alloc_chunk *) ((char *)x - CHUNK_OVERHEAD);
      size = ggc_get_size (x);
      if (fwrite (chunk, size + CHUNK_OVERHEAD, 1, f) != 1)
	fatal_error ("can't write PCH file: %m");
      d->written += size + CHUNK_OVERHEAD;
    }
   else
     {
       if (fwrite (x, size, 1, f) != 1)
	 fatal_error ("can't write PCH file: %m");
       d->written += size;
     }
  if (d->written == d->d.total
      && fseek (f, ROUND_UP_VALUE (d->d.total, G.pagesize), SEEK_CUR) != 0)
    fatal_error ("can't write PCH file: %m");
}

void
ggc_pch_finish (struct ggc_pch_data *d, FILE *f)
{
  if (fwrite (&d->d, sizeof (d->d), 1, f) != 1)
    fatal_error ("can't write PCH file: %m");
  free (d);
}
void
ggc_pch_read (FILE *f, void *addr)
{
  struct ggc_pch_ondisk d;
  struct page_entry *entry;
  struct alloc_zone *pch_zone;
  if (fread (&d, sizeof (d), 1, f) != 1)
    fatal_error ("can't read PCH file: %m");
  entry = xcalloc (1, sizeof (struct page_entry));
  entry->bytes = d.total;
  entry->page = addr;
  entry->context_depth = 0;
  pch_zone = new_ggc_zone ("PCH zone");
  entry->zone = pch_zone;
  entry->next = entry->zone->pages;
  entry->zone->pages = entry;
}
