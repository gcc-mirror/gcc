#include <stdlib.h>
#include <string.h>
#include <spu_cache.h>
extern void *malloc (__SIZE_TYPE__);
extern void *memset (void *, int, __SIZE_TYPE__);
extern void abort (void);

#ifdef __EA64__
#define addr unsigned long long
#else
#define addr unsigned long
#endif

#ifdef __EA64__
#define malloc_ea __malloc_ea64
#define memset_ea __memset_ea64
#define memcpy_ea __memcpy_ea64

typedef unsigned long long size_ea_t;

__ea void *__malloc_ea64 (size_ea_t);
__ea void *__memset_ea64 (__ea void *, int, size_ea_t);
__ea void *__memcpy_ea64 (__ea void *, __ea const void *, size_ea_t);
#define LINE_SIZE 128LL
#else
#define malloc_ea __malloc_ea32
#define memset_ea __memset_ea32
#define memcpy_ea __memcpy_ea32

typedef unsigned long size_ea_t;

__ea void *__malloc_ea32 (size_ea_t size);
__ea void *__memset_ea32 (__ea void *, int, size_ea_t);
__ea void *__memcpy_ea32 (__ea void *, __ea const void *, size_ea_t);
#define LINE_SIZE 128
#endif

static __ea void *bigblock;
static __ea void *block;
static int *ls_block;

extern void __cache_tag_array_size;
#define CACHE_SIZE (4 * (int) &__cache_tag_array_size)

void
init_mem (void)
{
  bigblock = malloc_ea (CACHE_SIZE + 2 * LINE_SIZE);
  block = malloc_ea (2 * LINE_SIZE);
  ls_block = malloc (LINE_SIZE);

  memset_ea (bigblock, 0, CACHE_SIZE + 2 * LINE_SIZE);
  memset_ea (block, -1, 2 * LINE_SIZE);
  memset (ls_block, -1, LINE_SIZE);
  cache_flush ();
}

/* Test 1: Simple cache fetching.  */
void
test1 (void)
{
  addr aligned = ((((addr) block) + LINE_SIZE - 1) & -LINE_SIZE);
  int *p1 = NULL;
  int *p2 = NULL;
  int i = 0;

  /* First, check if the same addr give the same cache ptr.  */
  p1 = cache_fetch ((__ea void *) aligned);
  p2 = cache_fetch ((__ea void *) aligned);

  if (p1 != p2)
    abort ();

  /* Check that the data actually is in the cache. */
  for (i = 0; i < LINE_SIZE / sizeof (int); i++)
    {
      if (p1[i] != -1)
	abort ();
    }

  /* Check returning within the cache line. */
  p2 = cache_fetch ((__ea void *) (aligned + sizeof (int)));

  if (p2 - p1 != 1)
    abort ();

  /* Finally, check that fetching an LS pointer returns that pointer.  */
  p1 = cache_fetch ((__ea char *) ls_block);
  if (p1 != ls_block)
    abort ();
}

/* Test 2: Eviction testing. */
void
test2 (void)
{
  addr aligned = ((((addr) block) + LINE_SIZE - 1) & -LINE_SIZE);
  int *p = NULL;
  int i = 0;

  /* First check that clean evictions don't write back.  */
  p = cache_fetch ((__ea void *) aligned);
  for (i = 0; i < LINE_SIZE / sizeof (int); i++)
    p[i] = 0;

  cache_evict ((__ea void *) aligned);
  memcpy_ea ((__ea char *) ls_block, (__ea void *) aligned, LINE_SIZE);

  for (i = 0; i < LINE_SIZE / sizeof (int); i++)
    {
      if (ls_block[i] == 0)
	abort ();
    }

  /* Now check that dirty evictions do write back.  */
  p = cache_fetch_dirty ((__ea void *) aligned, LINE_SIZE);
  for (i = 0; i < LINE_SIZE / sizeof (int); i++)
    p[i] = 0;

  cache_evict ((__ea void *) aligned);
  memcpy_ea ((__ea char *) ls_block, (__ea void *) aligned, LINE_SIZE);

  for (i = 0; i < LINE_SIZE / sizeof (int); i++)
    {
      if (ls_block[i] != 0)
	abort ();
    }

  /* Finally, check that non-atomic writeback only writes dirty bytes.  */

  for (i = 0; i < LINE_SIZE / sizeof (int); i++)
    {
      p = cache_fetch_dirty ((__ea void *) (aligned + i * sizeof (int)),
			     (i % 2) * sizeof (int));
      p[0] = -1;
    }

  cache_evict ((__ea void *) aligned);
  memcpy_ea ((__ea char *) ls_block, (__ea void *) aligned, LINE_SIZE);

  for (i = 0; i < LINE_SIZE / sizeof (int); i++)
    {
      if ((ls_block[i] == -1) && (i % 2 == 0))
	abort ();
      if ((ls_block[i] == 0) && (i % 2 == 1))
	abort ();
    }
}

/* Test LS forced-eviction. */
void
test3 (void)
{
  addr aligned = ((((addr) bigblock) + LINE_SIZE - 1) & -LINE_SIZE);
  char *test = NULL;
  char *ls = NULL;
  int i = 0;

  /* Init memory, fill the cache to capacity.  */
  ls = cache_fetch_dirty ((__ea void *) aligned, LINE_SIZE);
  for (i = 1; i < (CACHE_SIZE / LINE_SIZE); i++)
    cache_fetch_dirty ((__ea void *) (aligned + i * LINE_SIZE), LINE_SIZE);

  memset (ls, -1, LINE_SIZE);
  test = cache_fetch ((__ea void *) (aligned + CACHE_SIZE));

  /* test == ls indicates cache collision.  */
  if (test != ls)
    abort ();

  /* Make sure it actually wrote the cache line.  */
  for (i = 0; i < LINE_SIZE; i++)
    {
      if (ls[i] != 0)
	abort ();
    }

  ls = cache_fetch ((__ea void *) aligned);

  /* test != ls indicates another entry was evicted.  */
  if (test == ls)
    abort ();

  /* Make sure that the previous eviction actually wrote back.  */
  for (i = 0; i < LINE_SIZE; i++)
    {
      if (ls[i] != 0xFF)
	abort ();
    }
}

int
main (int argc, char **argv)
{
  init_mem ();
  test1 ();
  test2 ();
  test3 ();

  return 0;
}
