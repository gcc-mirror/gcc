/* PR tree-optimization/118224 */
/* { dg-do run } */
/* { dg-options "-O2 -w" } */

#include <stdlib.h>

void
foo (__SIZE_TYPE__ s)
{
  if (__builtin_calloc (s, ~(__SIZE_TYPE__) 0))
    __builtin_abort ();
  if (__builtin_calloc (~(__SIZE_TYPE__) 0, s))
    __builtin_abort ();
}

int
main ()
{
  if (__builtin_malloc (~(__SIZE_TYPE__) 0))
    __builtin_abort ();
#ifdef __GLIBC_PREREQ
#if __GLIBC_PREREQ (2, 16)
  /* aligned_alloc was added in glibc 2.16 */
  if (__builtin_aligned_alloc (32, ~(__SIZE_TYPE__) 0))
    __builtin_abort ();
#endif
#endif
  if (__builtin_calloc ((~(__SIZE_TYPE__) 0) / 2, 3))
    __builtin_abort ();
  if (__builtin_calloc ((~(__SIZE_TYPE__) 0) / 16, 64))
    __builtin_abort ();
  foo (1);
}
