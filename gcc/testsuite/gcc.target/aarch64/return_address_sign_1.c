/* Testing return address signing where no combined instructions used.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=pac-ret+leaf" } */
/* { dg-require-effective-target lp64 } */

int foo (int);

/* sibcall only.  */
int __attribute__ ((target ("arch=armv8.3-a")))
func1 (int a, int b)
{
  /* paciasp */
  return foo (a + b);
  /* autiasp */
}

/* non-leaf function with sibcall.  */
int __attribute__ ((target ("arch=armv8.3-a")))
func2 (int a, int b)
{
  /* paciasp */
  if (a < b)
    return b;

  a = foo (b);

  return foo (a);
  /* autiasp */
}

/* non-leaf function, legacy arch.  */
int __attribute__ ((target ("arch=armv8.2-a")))
func3 (int a, int b, int c)
{
  /* paciasp */
  return a + foo (b) + c;
  /* autiasp */
}

/* { dg-final { scan-assembler-times "paciasp" 3 } } */
/* { dg-final { scan-assembler-times "autiasp" 3 } } */
