/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -mtune=thunderx" } */
/* { dg-options "-O3 -mtune=thunderx -msve-vector-bits=256" { target aarch64_sve256_hw } } */

#include "peel_ind_2.c"

int __attribute__ ((optimize (1)))
main (void)
{
  foo ();
  for (int i = 0; i < N; ++i)
    {
      if (x[i] != (i < START || i >= END ? 0 : i))
	__builtin_abort ();
      asm volatile ("" ::: "memory");
    }
  return 0;
}
