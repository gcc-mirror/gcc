/* "Bag-of-pages" garbage collector for the GNU compiler.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "varray.h"
#include "flags.h"
#include "ggc.h"

#ifdef HAVE_MMAP_ANYWHERE
#include <sys/mman.h>
#endif

#ifndef MAP_FAILED
#define MAP_FAILED -1
#endif

#if !defined (MAP_ANONYMOUS) && defined (MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

/* Stategy: 

   This garbage-collecting allocator allocates objects on one of a set
   of pages.  Each page can allocate objects of a single size only;
   available sizes are powers of two starting at four bytes.  The size
   of an allocation request is rounded up to the next power of two
   (`order'), and satisfied from the appropriate page.

   Each page is recorded in a page-entry, which also maintains an
   in-use bitmap of object positions on the page.  This allows the
   allocation state of a particular object to be flipped without
   touching the page itself.

   Each page-entry also has a context depth, which is used to track
   pushing and popping of allocation contexts.  Only objects allocated
   in the current (highest-numbered) context may be collected.  

   Page entries are arranged in an array of singly-linked lists.  The
   array is indexed by the allocation size, in bits, of the pages on
   it; i.e. all pages on a list allocate objects of the same size.
   Pages are ordered on the list such that all non-full pages precede
   all full pages, with non-full pages arranged in order of decreasing
   context depth.

   Empty pages (of all orders) are kept on a single page cache list,
   and are considered first when new pages are required; they are
   deallocated at the start of the next collection if they haven't
   been recycled by then.  */


/* Define GGC_POISON to poison memory marked unused by the collector.  */
#undef GGC_POISON

/* Define GGC_ALWAYS_COLLECT to perform collection every time
   ggc_collect is invoked.  Otherwise, collection is performed only
   when a significant amount of memory has been allocated since the
   last collection.  */
#undef GGC_ALWAYS_COLLECT

#ifdef ENABLE_GC_CHECKING
#define GGC_POISON
#endif
#ifdef ENABLE_GC_ALWAYS_COLLECT
#define GGC_ALWAYS_COLLECT
#endif

/* Define GGC_DEBUG_LEVEL to print debugging information.
     0: No debugging output.
     1: GC statistics only.
     2: Page-entry allocations/deallocations as well.
     3: Object allocations as well.
     4: Object marks as well.   */
#define GGC_DEBUG_LEVEL (0)

#ifndef HOST_BITS_PER_PTR
#define HOST_BITS_PER_PTR  HOST_BITS_PER_LONG
#endif

/* Timing information for collect execution goes into here.  */
extern int gc_time;

/* The "" allocated string.  */
char *empty_string;

/* A two-level tree is used to look up the page-entry for a given
   pointer.  Two chunks of the pointer's bits are extracted to index
   the first and second levels of the tree, as follows:

				   HOST_PAGE_SIZE_BITS
			   32		|      |
       msb +----------------+----+------+------+ lsb
			    |    |      |
			 PAGE_L1_BITS   |
				 |      |
			       PAGE_L2_BITS

   The bottommost HOST_PAGE_SIZE_BITS are ignored, since page-entry
   pages are aligned on system page boundaries.  The next most
   significant PAGE_L2_BITS and PAGE_L1_BITS are the second and first
   index values in the lookup table, respectively.  

   For 32-bit architectures and the settings below, there are no
   leftover bits.  For architectures with wider pointers, the lookup
   tree points to a list of pages, which must be scanned to find the
   correct one.  */

#define PAGE_L1_BITS	(8)
#define PAGE_L2_BITS	(32 - PAGE_L1_BITS - G.lg_pagesize)
#define PAGE_L1_SIZE	((size_t) 1 << PAGE_L1_BITS)
#define PAGE_L2_SIZE	((size_t) 1 << PAGE_L2_BITS)

#define LOOKUP_L1(p) \
  (((size_t) (p) >> (32 - PAGE_L1_BITS)) & ((1 << PAGE_L1_BITS) - 1))

#define LOOKUP_L2(p) \
  (((size_t) (p) >> G.lg_pagesize) & ((1 << PAGE_L2_BITS) - 1))


/* A page_entry records the status of an allocation page.  This
   structure is dynamically sized to fit the bitmap in_use_p.  */
typedef struct page_entry 
{
  /* The next page-entry with objects of the same size, or NULL if
     this is the last page-entry.  */
  struct page_entry *next;

  /* The number of bytes allocated.  (This will always be a multiple
     of the host system page size.)  */
  size_t bytes;

  /* The address at which the memory is allocated.  */
  char *page;

  /* Saved in-use bit vector for pages that aren't in the topmost
     context during collection.  */
  unsigned long *save_in_use_p;

  /* Context depth of this page.  */
  unsigned char context_depth;

  /* The lg of size of objects allocated from this page.  */
  unsigned char order;

  /* The number of free objects remaining on this page.  */
  unsigned short num_free_objects;

  /* A likely candidate for the bit position of a free object for the
     next allocation from this page.  */
  unsigned short next_bit_hint;

  /* A bit vector indicating whether or not objects are in use.  The
     Nth bit is one if the Nth object on this page is allocated.  This
     array is dynamically sized.  */
  unsigned long in_use_p[1];
} page_entry;


#if HOST_BITS_PER_PTR <= 32

/* On 32-bit hosts, we use a two level page table, as pictured above.  */
typedef page_entry **page_table[PAGE_L1_SIZE];

#else

/* On 64-bit hosts, we use the same two level page tables plus a linked
   list that disambiguates the top 32-bits.  There will almost always be
   exactly one entry in the list.  */
typedef struct page_table_chain
{
  struct page_table_chain *next;
  size_t high_bits;
  page_entry **table[PAGE_L1_SIZE];
} *page_table;

#endif

/* The rest of the global variables.  */
static struct globals
{
  /* The Nth element in this array is a page with objects of size 2^N.
     If there are any pages with free objects, they will be at the
     head of the list.  NULL if there are no page-entries for this
     object size.  */
  page_entry *pages[HOST_BITS_PER_PTR];

  /* The Nth element in this array is the last page with objects of
     size 2^N.  NULL if there are no page-entries for this object
     size.  */
  page_entry *page_tails[HOST_BITS_PER_PTR];

  /* Lookup table for associating allocation pages with object addresses.  */
  page_table lookup;

  /* The system's page size.  */
  size_t pagesize;
  size_t lg_pagesize;

  /* Bytes currently allocated.  */
  size_t allocated;

  /* Bytes currently allocated at the end of the last collection.  */
  size_t allocated_last_gc;

  /* Total amount of memory mapped.  */
  size_t bytes_mapped;

  /* The current depth in the context stack.  */
  unsigned char context_depth;

  /* A file descriptor open to /dev/zero for reading.  */
#if defined (HAVE_MMAP_ANYWHERE) && !defined(MAP_ANONYMOUS)
  int dev_zero_fd;
#endif

  /* A cache of free system pages.  */
  page_entry *free_pages;

  /* The file descriptor for debugging output.  */
  FILE *debug_file;
} G;


/* Compute DIVIDEND / DIVISOR, rounded up.  */
#define DIV_ROUND_UP(Dividend, Divisor) \
  (((Dividend) + (Divisor) - 1) / (Divisor))

/* The number of objects per allocation page, for objects of size
   2^ORDER.  */
#define OBJECTS_PER_PAGE(Order) \
  ((Order) >= G.lg_pagesize ? 1 : G.pagesize / ((size_t)1 << (Order)))

/* The size in bytes required to maintain a bitmap for the objects
   on a page-entry.  */
#define BITMAP_SIZE(Num_objects) \
  (DIV_ROUND_UP ((Num_objects), HOST_BITS_PER_LONG) * sizeof(long))

/* Skip garbage collection if the current allocation is not at least
   this factor times the allocation at the end of the last collection.
   In other words, total allocation must expand by (this factor minus
   one) before collection is performed.  */
#define GGC_MIN_EXPAND_FOR_GC (1.3)

/* Bound `allocated_last_gc' to 4MB, to prevent the memory expansion
   test from triggering too often when the heap is small.  */
#define GGC_MIN_LAST_ALLOCATED (4 * 1024 * 1024)


static int ggc_allocated_p PROTO ((const void *));
static page_entry *lookup_page_table_entry PROTO ((const void *));
static void set_page_table_entry PROTO ((void *, page_entry *));
static char *alloc_anon PROTO ((char *, size_t));
static struct page_entry * alloc_page PROTO ((unsigned));
static void free_page PROTO ((struct page_entry *));
static void release_pages PROTO ((void));
static void clear_marks PROTO ((void));
static void sweep_pages PROTO ((void));
static void ggc_recalculate_in_use_p PROTO ((page_entry *));

#ifdef GGC_POISON
static void poison_pages PROTO ((void));
#endif

void debug_print_page_list PROTO ((int));

/* Returns non-zero if P was allocated in GC'able memory.  */

static inline int
ggc_allocated_p (p)
     const void *p;
{
  page_entry ***base;
  size_t L1, L2;

#if HOST_BITS_PER_PTR <= 32
  base = &G.lookup[0];
#else
  page_table table = G.lookup;
  size_t high_bits = (size_t) p & ~ (size_t) 0xffffffff;
  while (1)
    {
      if (table == NULL)
	return 0;
      if (table->high_bits == high_bits)
	break;
      table = table->next;
    }
  base = &table->table[0];
#endif

  /* Extract the level 1 and 2 indicies.  */
  L1 = LOOKUP_L1 (p);
  L2 = LOOKUP_L2 (p);

  return base[L1] && base[L1][L2];
}

/* Traverse the page table and find the entry for a page. 
   Die (probably) if the object wasn't allocated via GC.  */

static inline page_entry *
lookup_page_table_entry(p)
     const void *p;
{
  page_entry ***base;
  size_t L1, L2;

#if HOST_BITS_PER_PTR <= 32
  base = &G.lookup[0];
#else
  page_table table = G.lookup;
  size_t high_bits = (size_t) p & ~ (size_t) 0xffffffff;
  while (table->high_bits != high_bits)
    table = table->next;
  base = &table->table[0];
#endif

  /* Extract the level 1 and 2 indicies.  */
  L1 = LOOKUP_L1 (p);
  L2 = LOOKUP_L2 (p);

  return base[L1][L2];
}

/* Set the page table entry for a page.  */

static void
set_page_table_entry(p, entry)
     void *p;
     page_entry *entry;
{
  page_entry ***base;
  size_t L1, L2;

#if HOST_BITS_PER_PTR <= 32
  base = &G.lookup[0];
#else
  page_table table;
  size_t high_bits = (size_t) p & ~ (size_t) 0xffffffff;
  for (table = G.lookup; table; table = table->next)
    if (table->high_bits == high_bits)
      goto found;

  /* Not found -- allocate a new table.  */
  table = (page_table) xcalloc (1, sizeof(*table));
  table->next = G.lookup;
  table->high_bits = high_bits;
  G.lookup = table;
found:
  base = &table->table[0];
#endif

  /* Extract the level 1 and 2 indicies.  */
  L1 = LOOKUP_L1 (p);
  L2 = LOOKUP_L2 (p);

  if (base[L1] == NULL)
    base[L1] = (page_entry **) xcalloc (PAGE_L2_SIZE, sizeof (page_entry *));

  base[L1][L2] = entry;
}

/* Prints the page-entry for object size ORDER, for debugging.  */

void
debug_print_page_list (order)
     int order;
{
  page_entry *p;
  printf ("Head=%p, Tail=%p:\n", G.pages[order], G.page_tails[order]);
  p = G.pages[order];
  while (p != NULL)
    {
      printf ("%p(%1d|%3d) -> ", p, p->context_depth, p->num_free_objects);
      p = p->next;
    }
  printf ("NULL\n");
  fflush (stdout);
}

/* Allocate SIZE bytes of anonymous memory, preferably near PREF,
   (if non-null).  */

static inline char *
alloc_anon (pref, size)
     char *pref ATTRIBUTE_UNUSED;
     size_t size;
{
  char *page;

#ifdef HAVE_MMAP_ANYWHERE
#ifdef MAP_ANONYMOUS
  page = (char *) mmap (pref, size, PROT_READ | PROT_WRITE,
			MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#else
  page = (char *) mmap (pref, size, PROT_READ | PROT_WRITE,
			MAP_PRIVATE, G.dev_zero_fd, 0);
#endif
  if (page == (char *) MAP_FAILED)
    {
      fputs ("Virtual memory exhausted!\n", stderr);
      exit(1);
    }
#else
#ifdef HAVE_VALLOC
  page = (char *) valloc (size);
  if (!page)
    {
      fputs ("Virtual memory exhausted!\n", stderr);
      exit(1);
    }
#endif /* HAVE_VALLOC */
#endif /* HAVE_MMAP_ANYWHERE */

  /* Remember that we allocated this memory.  */
  G.bytes_mapped += size;

  return page;
}

/* Allocate a new page for allocating objects of size 2^ORDER,
   and return an entry for it.  The entry is not added to the
   appropriate page_table list.  */

static inline struct page_entry *
alloc_page (order)
     unsigned order;
{
  struct page_entry *entry, *p, **pp;
  char *page;
  size_t num_objects;
  size_t bitmap_size;
  size_t page_entry_size;
  size_t entry_size;

  num_objects = OBJECTS_PER_PAGE (order);
  bitmap_size = BITMAP_SIZE (num_objects + 1);
  page_entry_size = sizeof (page_entry) - sizeof (long) + bitmap_size;
  entry_size = num_objects * (1 << order);

  entry = NULL;
  page = NULL;

  /* Check the list of free pages for one we can use.  */
  for (pp = &G.free_pages, p = *pp; p ; pp = &p->next, p = *pp)
    if (p->bytes == entry_size)
      break;

  if (p != NULL)
    {
      /* Recycle the allocated memory from this page ... */
      *pp = p->next;
      page = p->page;
      /* ... and, if possible, the page entry itself.  */
      if (p->order == order)
	{
	  entry = p;
	  memset (entry, 0, page_entry_size);
	}
      else
	free (p);
    }
  else
    {
      /* Actually allocate the memory.  */
      page = alloc_anon (NULL, entry_size);
    }

  if (entry == NULL)
    entry = (struct page_entry *) xcalloc (1, page_entry_size);

  entry->bytes = entry_size;
  entry->page = page;
  entry->context_depth = G.context_depth;
  entry->order = order;
  entry->num_free_objects = num_objects;
  entry->next_bit_hint = 1;

  /* Set the one-past-the-end in-use bit.  This acts as a sentry as we
     increment the hint.  */
  entry->in_use_p[num_objects / HOST_BITS_PER_LONG]
    = (unsigned long) 1 << (num_objects % HOST_BITS_PER_LONG);

  set_page_table_entry (page, entry);

  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file, 
	     "Allocating page at %p, object size=%d, data %p-%p\n", entry,
	     1 << order, page, page + entry_size - 1);

  return entry;
}

/* For a page that is no longer needed, put it on the free page list.  */

static inline void
free_page (entry)
     page_entry *entry;
{
  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file, 
	     "Deallocating page at %p, data %p-%p\n", entry,
	     entry->page, entry->page + entry->bytes - 1);

  set_page_table_entry (entry->page, NULL);

  entry->next = G.free_pages;
  G.free_pages = entry;
}

/* Release the free page cache to the system.  */

static void
release_pages ()
{
#ifdef HAVE_MMAP_ANYWHERE
  page_entry *p, *next;
  char *start;
  size_t len;

  p = G.free_pages;
  if (p == NULL)
    return;

  next = p->next;
  start = p->page;
  len = p->bytes;
  free (p);
  p = next;

  while (p)
    {
      next = p->next;
      /* Gather up adjacent pages so they are unmapped together.  */
      if (p->page == start + len)
	len += p->bytes;
      else
	{
	  munmap (start, len);
	  G.bytes_mapped -= len;
	  start = p->page;
	  len = p->bytes;
	}
      free (p);
      p = next;
    }

  munmap (start, len);
  G.bytes_mapped -= len;
#else
#ifdef HAVE_VALLOC
  page_entry *p, *next;

  for (p = G.free_pages; p ; p = next)
    {
      next = p->next;
      free (p->page);
      G.bytes_mapped -= p->bytes;
      free (p);
    }
#endif /* HAVE_VALLOC */
#endif /* HAVE_MMAP_ANYWHERE */

  G.free_pages = NULL;
}

/* This table provides a fast way to determine ceil(log_2(size)) for
   allocation requests.  The minimum allocation size is four bytes.  */

static unsigned char const size_lookup[257] = 
{ 
  2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 
  4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 
  5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
  6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 
  7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8
};

/* Allocate a chunk of memory of SIZE bytes.  If ZERO is non-zero, the
   memory is zeroed; otherwise, its contents are undefined.  */

void *
ggc_alloc_obj (size, zero)
     size_t size;
     int zero;
{
  unsigned order, word, bit, object_offset;
  struct page_entry *entry;
  void *result;

  if (size <= 256)
    order = size_lookup[size];
  else
    {
      order = 9;
      while (size > ((size_t) 1 << order))
	order++;
    }

  /* If there are non-full pages for this size allocation, they are at
     the head of the list.  */
  entry = G.pages[order];

  /* If there is no page for this object size, or all pages in this
     context are full, allocate a new page.  */
  if (entry == NULL || entry->num_free_objects == 0)
    {
      struct page_entry *new_entry;
      new_entry = alloc_page (order);
      
      /* If this is the only entry, it's also the tail.  */
      if (entry == NULL)
	G.page_tails[order] = new_entry;
     
      /* Put new pages at the head of the page list.  */
      new_entry->next = entry;
      entry = new_entry;
      G.pages[order] = new_entry;

      /* For a new page, we know the word and bit positions (in the
	 in_use bitmap) of the first available object -- they're zero.  */
      new_entry->next_bit_hint = 1;
      word = 0;
      bit = 0;
      object_offset = 0;
    }
  else
    {
      /* First try to use the hint left from the previous allocation
	 to locate a clear bit in the in-use bitmap.  We've made sure
	 that the one-past-the-end bit is always set, so if the hint
	 has run over, this test will fail.  */
      unsigned hint = entry->next_bit_hint;
      word = hint / HOST_BITS_PER_LONG;
      bit = hint % HOST_BITS_PER_LONG;
      
      /* If the hint didn't work, scan the bitmap from the beginning.  */
      if ((entry->in_use_p[word] >> bit) & 1)
	{
	  word = bit = 0;
	  while (~entry->in_use_p[word] == 0)
	    ++word;
	  while ((entry->in_use_p[word] >> bit) & 1)
	    ++bit;
	  hint = word * HOST_BITS_PER_LONG + bit;
	}

      /* Next time, try the next bit.  */
      entry->next_bit_hint = hint + 1;

      object_offset = hint << order;
    }

  /* Set the in-use bit.  */
  entry->in_use_p[word] |= ((unsigned long) 1 << bit);

  /* Keep a running total of the number of free objects.  If this page
     fills up, we may have to move it to the end of the list if the
     next page isn't full.  If the next page is full, all subsequent
     pages are full, so there's no need to move it.  */
  if (--entry->num_free_objects == 0
      && entry->next != NULL
      && entry->next->num_free_objects > 0)
    {
      G.pages[order] = entry->next;
      entry->next = NULL;
      G.page_tails[order]->next = entry;
      G.page_tails[order] = entry;
    }

  /* Calculate the object's address.  */
  result = entry->page + object_offset;

#ifdef GGC_POISON
  /* `Poison' the entire allocated object before zeroing the requested area,
     so that bytes beyond the end, if any, will not necessarily be zero.  */
  memset (result, 0xaf, 1 << order);
#endif

  if (zero)
    memset (result, 0, size);

  /* Keep track of how many bytes are being allocated.  This
     information is used in deciding when to collect.  */
  G.allocated += (size_t) 1 << order;

  if (GGC_DEBUG_LEVEL >= 3)
    fprintf (G.debug_file, 
	     "Allocating object, requested size=%d, actual=%d at %p on %p\n",
	     (int) size, 1 << order, result, entry);

  return result;
}

/* If P is not marked, marks it and return false.  Otherwise return true.
   P must have been allocated by the GC allocator; it mustn't point to
   static objects, stack variables, or memory allocated with malloc.  */

int
ggc_set_mark (p)
     void *p;
{
  page_entry *entry;
  unsigned bit, word;
  unsigned long mask;

  /* Look up the page on which the object is alloced.  If the object
     wasn't allocated by the collector, we'll probably die.  */
  entry = lookup_page_table_entry (p);
#ifdef ENABLE_CHECKING
  if (entry == NULL)
    abort ();
#endif

  /* Calculate the index of the object on the page; this is its bit
     position in the in_use_p bitmap.  */
  bit = (((char *) p) - entry->page) >> entry->order;
  word = bit / HOST_BITS_PER_LONG;
  mask = (unsigned long) 1 << (bit % HOST_BITS_PER_LONG);
  
  /* If the bit was previously set, skip it. */
  if (entry->in_use_p[word] & mask)
    return 1;

  /* Otherwise set it, and decrement the free object count.  */
  entry->in_use_p[word] |= mask;
  entry->num_free_objects -= 1;

  G.allocated += (size_t) 1 << entry->order;

  if (GGC_DEBUG_LEVEL >= 4)
    fprintf (G.debug_file, "Marking %p\n", p);

  return 0;
}

/* Mark P, but check first that it was allocated by the collector.  */

void
ggc_mark_if_gcable (p)
     void *p;
{
  if (p && ggc_allocated_p (p))
    ggc_set_mark (p);
}

/* Return the size of the gc-able object P.  */

size_t
ggc_get_size (p)
     void *p;
{
  page_entry *pe = lookup_page_table_entry (p);
  return 1 << pe->order;
}

/* Initialize the ggc-mmap allocator.  */

void
init_ggc ()
{
  G.pagesize = getpagesize();
  G.lg_pagesize = exact_log2 (G.pagesize);

#if defined (HAVE_MMAP_ANYWHERE) && !defined(MAP_ANONYMOUS)
  G.dev_zero_fd = open ("/dev/zero", O_RDONLY);
  if (G.dev_zero_fd == -1)
    abort ();
#endif

#if 0
  G.debug_file = fopen ("ggc-mmap.debug", "w");
#else
  G.debug_file = stdout;
#endif

  G.allocated_last_gc = GGC_MIN_LAST_ALLOCATED;

#ifdef HAVE_MMAP_ANYWHERE
  /* StunOS has an amazing off-by-one error for the first mmap allocation
     after fiddling with RLIMIT_STACK.  The result, as hard as it is to
     believe, is an unaligned page allocation, which would cause us to
     hork badly if we tried to use it.  */
  {
    char *p = alloc_anon (NULL, G.pagesize);
    if ((size_t)p & (G.pagesize - 1))
      {
	/* How losing.  Discard this one and try another.  If we still
	   can't get something useful, give up.  */

	p = alloc_anon (NULL, G.pagesize);
	if ((size_t)p & (G.pagesize - 1))
	  abort ();
      }
    munmap (p, G.pagesize);
  }
#endif

  empty_string = ggc_alloc_string ("", 0);
  ggc_add_string_root (&empty_string, 1);
}

/* Increment the `GC context'.  Objects allocated in an outer context
   are never freed, eliminating the need to register their roots.  */

void
ggc_push_context ()
{
  ++G.context_depth;

  /* Die on wrap.  */
  if (G.context_depth == 0)
    abort ();
}

/* Merge the SAVE_IN_USE_P and IN_USE_P arrays in P so that IN_USE_P
   reflects reality.  Recalculate NUM_FREE_OBJECTS as well.  */

static void
ggc_recalculate_in_use_p (p)
     page_entry *p;
{
  unsigned int i;
  size_t num_objects;

  /* Because the past-the-end bit in in_use_p is always set, we 
     pretend there is one additional object.  */
  num_objects = OBJECTS_PER_PAGE (p->order) + 1;

  /* Reset the free object count.  */
  p->num_free_objects = num_objects;

  /* Combine the IN_USE_P and SAVE_IN_USE_P arrays.  */
  for (i = 0; 
       i < DIV_ROUND_UP (BITMAP_SIZE (num_objects),
			 sizeof (*p->in_use_p));
       ++i)
    {
      unsigned long j;

      /* Something is in use if it is marked, or if it was in use in a
	 context further down the context stack.  */
      p->in_use_p[i] |= p->save_in_use_p[i];

      /* Decrement the free object count for every object allocated.  */
      for (j = p->in_use_p[i]; j; j >>= 1)
	p->num_free_objects -= (j & 1);
    }

  if (p->num_free_objects >= num_objects)
    abort ();
}

/* Decrement the `GC context'.  All objects allocated since the 
   previous ggc_push_context are migrated to the outer context.  */

void
ggc_pop_context ()
{
  unsigned order, depth;

  depth = --G.context_depth;

  /* Any remaining pages in the popped context are lowered to the new
     current context; i.e. objects allocated in the popped context and
     left over are imported into the previous context.  */
  for (order = 2; order < HOST_BITS_PER_PTR; order++)
    {
      page_entry *p;

      for (p = G.pages[order]; p != NULL; p = p->next)
	{
	  if (p->context_depth > depth)
	    p->context_depth = depth;

	  /* If this page is now in the topmost context, and we'd
	     saved its allocation state, restore it.  */
	  else if (p->context_depth == depth && p->save_in_use_p)
	    {
	      ggc_recalculate_in_use_p (p);
	      free (p->save_in_use_p);
	      p->save_in_use_p = 0;
	    }
	}
    }
}

/* Unmark all objects.  */

static inline void
clear_marks ()
{
  unsigned order;

  for (order = 2; order < HOST_BITS_PER_PTR; order++)
    {
      size_t num_objects = OBJECTS_PER_PAGE (order);
      size_t bitmap_size = BITMAP_SIZE (num_objects + 1);
      page_entry *p;

      for (p = G.pages[order]; p != NULL; p = p->next)
	{
#ifdef ENABLE_CHECKING
	  /* The data should be page-aligned.  */
	  if ((size_t) p->page & (G.pagesize - 1))
	    abort ();
#endif

	  /* Pages that aren't in the topmost context are not collected;
	     nevertheless, we need their in-use bit vectors to store GC
	     marks.  So, back them up first.  */
	  if (p->context_depth < G.context_depth)
	    {
	      if (! p->save_in_use_p)
		p->save_in_use_p = xmalloc (bitmap_size);
	      memcpy (p->save_in_use_p, p->in_use_p, bitmap_size);
	    }

	  /* Reset reset the number of free objects and clear the
             in-use bits.  These will be adjusted by mark_obj.  */
	  p->num_free_objects = num_objects;
	  memset (p->in_use_p, 0, bitmap_size);

	  /* Make sure the one-past-the-end bit is always set.  */
	  p->in_use_p[num_objects / HOST_BITS_PER_LONG] 
	    = ((unsigned long) 1 << (num_objects % HOST_BITS_PER_LONG));
	}
    }
}

/* Free all empty pages.  Partially empty pages need no attention
   because the `mark' bit doubles as an `unused' bit.  */

static inline void
sweep_pages ()
{
  unsigned order;

  for (order = 2; order < HOST_BITS_PER_PTR; order++)
    {
      /* The last page-entry to consider, regardless of entries
	 placed at the end of the list.  */
      page_entry * const last = G.page_tails[order];

      size_t num_objects = OBJECTS_PER_PAGE (order);
      page_entry *p, *previous;
      int done;
	
      p = G.pages[order];
      if (p == NULL)
	continue;

      previous = NULL;
      do
	{
	  page_entry *next = p->next;

	  /* Loop until all entries have been examined.  */
	  done = (p == last);

	  /* Only objects on pages in the topmost context should get
	     collected.  */
	  if (p->context_depth < G.context_depth)
	    ;

	  /* Remove the page if it's empty.  */
	  else if (p->num_free_objects == num_objects)
	    {
	      if (! previous)
		G.pages[order] = next;
	      else
		previous->next = next;

	      /* Are we removing the last element?  */
	      if (p == G.page_tails[order])
		G.page_tails[order] = previous;
	      free_page (p);
	      p = previous;
	    }

	  /* If the page is full, move it to the end.  */
	  else if (p->num_free_objects == 0)
	    {
	      /* Don't move it if it's already at the end.  */
	      if (p != G.page_tails[order])
		{
		  /* Move p to the end of the list.  */
		  p->next = NULL;
		  G.page_tails[order]->next = p;

		  /* Update the tail pointer...  */
		  G.page_tails[order] = p;

		  /* ... and the head pointer, if necessary.  */
		  if (! previous)
		    G.pages[order] = next;
		  else
		    previous->next = next;
		  p = previous;
		}
	    }

	  /* If we've fallen through to here, it's a page in the
	     topmost context that is neither full nor empty.  Such a
	     page must precede pages at lesser context depth in the
	     list, so move it to the head.  */
	  else if (p != G.pages[order])
	    {
	      previous->next = p->next;
	      p->next = G.pages[order];
	      G.pages[order] = p;
	      /* Are we moving the last element?  */
	      if (G.page_tails[order] == p)
	        G.page_tails[order] = previous;
	      p = previous;
	    }

	  previous = p;
	  p = next;
	} 
      while (! done);

      /* Now, restore the in_use_p vectors for any pages from contexts
         other than the current one.  */
      for (p = G.pages[order]; p; p = p->next)
	if (p->context_depth != G.context_depth)
	  ggc_recalculate_in_use_p (p);
    }
}

#ifdef GGC_POISON
/* Clobber all free objects.  */

static inline void
poison_pages ()
{
  unsigned order;

  for (order = 2; order < HOST_BITS_PER_PTR; order++)
    {
      size_t num_objects = OBJECTS_PER_PAGE (order);
      size_t size = (size_t) 1 << order;
      page_entry *p;

      for (p = G.pages[order]; p != NULL; p = p->next)
	{
	  size_t i;

	  if (p->context_depth != G.context_depth)
	    /* Since we don't do any collection for pages in pushed
	       contexts, there's no need to do any poisoning.  And
	       besides, the IN_USE_P array isn't valid until we pop
	       contexts.  */
	    continue;

	  for (i = 0; i < num_objects; i++)
	    {
	      size_t word, bit;
	      word = i / HOST_BITS_PER_LONG;
	      bit = i % HOST_BITS_PER_LONG;
	      if (((p->in_use_p[word] >> bit) & 1) == 0)
		memset (p->page + i * size, 0xa5, size);
	    }
	}
    }
}
#endif

/* Top level mark-and-sweep routine.  */

void
ggc_collect ()
{
  int time;

  /* Avoid frequent unnecessary work by skipping collection if the
     total allocations haven't expanded much since the last
     collection.  */
#ifndef GGC_ALWAYS_COLLECT
  if (G.allocated < GGC_MIN_EXPAND_FOR_GC * G.allocated_last_gc)
    return;
#endif

  time = get_run_time ();
  if (!quiet_flag)
    fprintf (stderr, " {GC %luk -> ", (unsigned long)G.allocated / 1024);

  /* Zero the total allocated bytes.  We'll reaccumulate this while
     marking.  */
  G.allocated = 0;

  /* Release the pages we freed the last time we collected, but didn't 
     reuse in the interim.  */
  release_pages ();

  clear_marks ();
  ggc_mark_roots ();
  
#ifdef GGC_POISON
  poison_pages ();
#endif

  sweep_pages ();

  G.allocated_last_gc = G.allocated;
  if (G.allocated_last_gc < GGC_MIN_LAST_ALLOCATED)
    G.allocated_last_gc = GGC_MIN_LAST_ALLOCATED;

  time = get_run_time () - time;
  gc_time += time;

  if (!quiet_flag)
    {
      fprintf (stderr, "%luk in %.3f}", 
	       (unsigned long) G.allocated / 1024, time * 1e-6);
    }
}

/* Print allocation statistics.  */

void
ggc_page_print_statistics ()
{
  struct ggc_statistics stats;
  unsigned int i;

  /* Clear the statistics.  */
  bzero (&stats, sizeof (stats));
  
  /* Make sure collection will really occur.  */
  G.allocated_last_gc = 0;

  /* Collect and print the statistics common across collectors.  */
  ggc_print_statistics (stderr, &stats);

  /* Release free pages so that we will not count the bytes allocated
     there as part of the total allocated memory.  */
  release_pages ();

  /* Collect some information about the various sizes of 
     allocation.  */
  fprintf (stderr, "\n%-4s%-16s%-16s\n", "Log", "Allocated", "Used");
  for (i = 0; i < HOST_BITS_PER_PTR; ++i)
    {
      page_entry *p;
      size_t allocated;
      size_t in_use;

      /* Skip empty entries.  */
      if (!G.pages[i])
	continue;

      allocated = in_use = 0;

      /* Figure out the total number of bytes allocated for objects of
	 this size, and how many of them are actually in use.  */
      for (p = G.pages[i]; p; p = p->next)
	{
	  allocated += p->bytes;
	  in_use += 
	    (OBJECTS_PER_PAGE (i) - p->num_free_objects) * (1 << i);
	}
      fprintf (stderr, "%-3d %-15lu %-15lu\n", i, 
	       (unsigned long) allocated, (unsigned long) in_use);
    }

  /* Print out some global information.  */
  fprintf (stderr, "\nTotal bytes marked: %lu\n", 
	   (unsigned long) G.allocated);
  fprintf (stderr, "Total bytes mapped: %lu\n", 
	   (unsigned long) G.bytes_mapped);
}
