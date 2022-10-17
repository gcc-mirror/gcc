/* { dg-do run { target { power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */
#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#endif

extern void abort (void);

int
main ()
{
  vector unsigned long long arg1, arg2;
  vector unsigned __int128 arg3, result, expected;
  unsigned __int128 c = (unsigned __int128) (-1); /* 2^128 - 1 */

  arg1 = (vector unsigned long long) { 111ULL, 300ULL };
  arg2 = (vector unsigned long long) { 700ULL, 222ULL };
  arg3 = (vector unsigned __int128) { c };
  expected = (vector unsigned __int128) { 1 };

  result = vec_msumc (arg1, arg2, arg3);
  if (result[0] != expected[0])
    {
#if DEBUG
      printf ("ERROR, expected %d, result %d\n",
	      (unsigned int) expected[0],
	      (unsigned int) result[0]);
#else
      abort ();
#endif
    }

  return 0;
}
