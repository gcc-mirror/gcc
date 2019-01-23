/* Testing the disable of return address signing.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=pac-ret+leaf" } */
/* { dg-require-effective-target lp64 } */

int bar (int, int);

int __attribute__ ((target ("arch=armv8.3-a, branch-protection=pac-ret")))
func1_leaf (int a, int b, int c, int d)
{
  return a + b + c + d;
}

int __attribute__ ((target ("arch=armv8.3-a, branch-protection=none")))
func2_none (int a, int b, int c, int d)
{
  return c + bar (a, b) + d;
}

/* { dg-final { scan-assembler-not "paciasp" } } */
/* { dg-final { scan-assembler-not "autiasp" } } */
/* { dg-final { scan-assembler-not "retaa" } } */
