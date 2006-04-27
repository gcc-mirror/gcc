/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse" } */
#include <xmmintrin.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "../../gcc.dg/i386-cpuid.h"

int
main()
{
  int alignment, n;
  void *ptr;
  int errors = 0;
  const char test [] = "This is a test.";
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if ((cpu_facilities & bit_SSE) != bit_SSE )
    /* If host has no vector support, pass.  */
    exit (0);

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
  exit (0);
}
