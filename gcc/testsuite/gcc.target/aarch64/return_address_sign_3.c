/* Testing the disable of return address signing.  */
/* { dg-do compile } */
/* { dg-options "-O2 -msign-return-address=all" } */
/* { dg-require-effective-target lp64 } */

int bar (int, int);

int __attribute__ ((target ("arch=armv8.3-a, sign-return-address=non-leaf")))
func1_leaf (int a, int b, int c, int d)
{
  return a + b + c + d;
}

int __attribute__ ((target ("arch=armv8.3-a, sign-return-address=none")))
func2_none (int a, int b, int c, int d)
{
  return c + bar (a, b) + d;
}

/* { dg-final { scan-assembler-not "paciasp" } } */
/* { dg-final { scan-assembler-not "autiasp" } } */
/* { dg-final { scan-assembler-not "retaa" } } */
