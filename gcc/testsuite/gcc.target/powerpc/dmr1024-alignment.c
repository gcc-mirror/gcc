/* { dg-do run } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -mhard-float" } */

/* Verify the __dmr1024 opaque type is always registered (regardless of
   whether -mdense-math is enabled) and has the expected size and
   alignment, following the same model as __vector_pair and
   __vector_quad in mma-alignment.c.  */

#include <stdlib.h>

struct
{
  int __attribute__ ((__aligned__)) ivar;
  __dmr1024 dmr;
} s;

int
main (void)
{
  /* __dmr1024 holds the contents of a 1,024-bit Dense Math Register.  */
  if (sizeof (__dmr1024) != 128)
    abort ();

  /* __dmr1024 loads/stores are always broken into two vector-pair
     transfers, and there is no support for aligning the stack to
     1,024 bits, so the type alignment is 512 bits (64 bytes), matching
     __vector_quad rather than its own 1,024-bit size.  */
  if (__alignof__ (s.dmr) != 64)
    abort ();

  return 0;
}
