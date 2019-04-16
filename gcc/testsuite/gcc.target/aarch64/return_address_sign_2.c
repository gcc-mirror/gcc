/* Testing return address signing where combined instructions used.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=pac-ret+leaf" } */
/* { dg-require-effective-target lp64 } */

int foo (int);
int bar (int, int);

int __attribute__ ((target ("arch=armv8.3-a")))
func1 (int a, int b, int c)
{
  /* paciasp */
  return a + foo (b) + c;
  /* retaa */
}

/* { dg-final { scan-assembler-times "paciasp" 1 } } */
/* { dg-final { scan-assembler-times "retaa" 1 } } */
