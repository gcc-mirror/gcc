/* Testing return address signing where no combined instructions used.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=pac-ret+leaf+b-key" } */
/* { dg-require-effective-target lp64 } */

int foo (int);

/* sibcall only.  */
int __attribute__ ((target ("arch=armv8.3-a")))
func1 (int a, int b)
{
  /* pacibsp */
  return foo (a + b);
  /* autibsp */
}

/* non-leaf function with sibcall.  */
int __attribute__ ((target ("arch=armv8.3-a")))
func2 (int a, int b)
{
  /* pacibsp */
  if (a < b)
    return b;

  a = foo (b);

  return foo (a);
  /* autibsp */
}

/* non-leaf function, legacy arch.  */
int __attribute__ ((target ("arch=armv8.2-a")))
func3 (int a, int b, int c)
{
  /* pacibsp */
  return a + foo (b) + c;
  /* autibsp */
}

/* { dg-final { scan-assembler-times "pacibsp" 3 } } */
/* { dg-final { scan-assembler-times "autibsp" 3 } } */
