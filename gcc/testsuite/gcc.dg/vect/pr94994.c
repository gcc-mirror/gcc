#include <stdint.h>
#include "tree-vect.h"

#define BLOCK_SIZE (sizeof (uint32_t))

struct unaligned {
  uint32_t x;
} __attribute__((packed, may_alias));

static inline uint32_t
load_unaligned (const char *p)
{
  return ((struct unaligned *) p)->x;
}

static inline void
store_unaligned (uint32_t x, char *p)
{
  ((struct unaligned *) p)->x = x;
}

void __attribute__((noipa))
copy (char *dst, const char *src, size_t n)
{
  for (size_t i = 0; i < n; i += BLOCK_SIZE)
    store_unaligned (load_unaligned (src + i), dst + i);
}

#define INPUT_SIZE 64
#define MAX_STEP 32

char x[INPUT_SIZE + MAX_STEP];

int
main (void)
{
  check_vect ();

  for (unsigned int i = 1; i < MAX_STEP; ++i)
    {
      for (unsigned int j = 0; j < INPUT_SIZE + MAX_STEP; ++j)
	x[j] = j + 10;
      copy (x + i, x, INPUT_SIZE);
      for (int j = 0; j < INPUT_SIZE + i; ++j)
	{
	  int expected;
	  if (j < i)
	    expected = j + 10;
	  else if (i >= BLOCK_SIZE)
	    expected = j % i + 10;
	  else if ((j - i) % BLOCK_SIZE < i)
	    expected = x[j - i];
	  else
	    expected = j - i + 10;
	  if (x[j] != expected)
	    __builtin_abort ();
	}
    }

  return 0;
}
