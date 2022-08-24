/* { dg-do run { target { powerpc*-*-* && htm_hw } } } */
/* { dg-require-effective-target powerpc_htm_ok } */
/* { dg-options "-mhtm" } */

/* Program to test PowerPC HTM instructions.  */

#include <stdlib.h>
#include <htmintrin.h>

int
main (void)
{
  long i;
  unsigned long mask = 0;
  unsigned long retry_count = 0;

repeat:
  if (__builtin_tbegin (0))
    {
      mask++;
      retry_count = 0;
    }
  else
    {
      /* Retry a limited number of times before aborting.  */
      if (retry_count++ < 10)
	goto repeat;
      abort ();
    }

  if (mask == 1)
    {
      __builtin_tsuspend ();

      if (_HTM_STATE (__builtin_tcheck ()) != _HTM_SUSPENDED)
	abort ();

      __builtin_tresume ();

      if (_HTM_STATE (__builtin_tcheck ()) != _HTM_TRANSACTIONAL)
	abort ();
    }
  else
    mask++;

  if (_HTM_STATE (__builtin_tendall ()) != _HTM_TRANSACTIONAL)
    abort ();

  if (mask == 1)
    goto repeat;

  if (_HTM_STATE (__builtin_tendall ()) != _HTM_NONTRANSACTIONAL)
    abort ();

  if (mask != 3)
    abort ();

  return 0;
}
