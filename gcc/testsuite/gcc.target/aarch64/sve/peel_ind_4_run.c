/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast -mtune=thunderx" } */
/* { dg-options "-Ofast -mtune=thunderx -mtune=thunderx" { target aarch64_sve256_hw } } */

#include "peel_ind_4.c"

int __attribute__ ((optimize (1)))
main (void)
{
  double x[END + 1];
  for (int i = 0; i < END + 1; ++i)
    {
      x[i] = i;
      asm volatile ("" ::: "memory");
    }
  foo (x);
  for (int i = 0; i < END + 1; ++i)
    {
      double expected;
      if (i < START || i >= END)
	expected = i;
      else
	expected = 10 + (i - START) * 5;
      if (x[i] != expected)
	__builtin_abort ();
      asm volatile ("" ::: "memory");
    }
  return 0;
}
