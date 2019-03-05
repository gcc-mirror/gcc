/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target aarch64_bti_hw } */
/* If configured with --enable-standard-branch-protection, don't use
   command line option.  */
/* { dg-additional-options "-mbranch-protection=standard" { target { ! default_branch_protection } } } */

#include<stdio.h>

typedef int FP (int);

int
f1 (FP fp, int n)
{
  return (fp) (n);
}

int
f2 (int n, FP fp)
{
  return (fp) (n);
}

int __attribute__ ((noinline))
func (int x)
{
  return x+1;
}

int main ()
{
  int s = 0;
  s += f1 (func, 10);
  s += f2 (s, func);
  printf ("S: %d\n", s);
  return !(s == 23);
}
