/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -mtune=thunderx" } */
/* { dg-options "-O3 -mtune=thunderx -msve-vector-bits=256" { target aarch64_sve256_hw } } */

#include "peel_ind_3.c"

int __attribute__ ((optimize (1)))
main (void)
{
  for (int start = 0; start < MAX_START; ++start)
    {
      foo (start);
      for (int i = 0; i < N; ++i)
	{
	  if (x[start][i] != (i < start || i >= start + COUNT ? 0 : i))
	    __builtin_abort ();
	  asm volatile ("" ::: "memory");
	}
    }
  return 0;
}
