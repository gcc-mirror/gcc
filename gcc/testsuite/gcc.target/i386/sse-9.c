/* { dg-do run } */
/* { dg-options "-O2 -msse" } */

#include "sse-check.h"

#include <xmmintrin.h>
#include <stddef.h>
#include <string.h>

static void
sse_test (void)
{
  int alignment, n;
  void *ptr;
  int errors = 0;
  const char test [] = "This is a test.";

  for (alignment = 1; alignment <= (1 << 20); alignment += alignment)
    {
      ptr = _mm_malloc (alignment, alignment);
      if (((ptrdiff_t) ptr) & (alignment - 1))
	abort ();
      if (ptr)
	{
	  n = alignment > sizeof test ? sizeof test : alignment;
	  memcpy (ptr, test, n);
	  if (memcmp (ptr, test, n) != 0)
	    errors++;
	  _mm_free (ptr);
	}
      else
	errors++;
    }
   
  if (errors != 0)
    abort ();
}
