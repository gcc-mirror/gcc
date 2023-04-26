/* Testing return address signing where combined instructions used.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=pac-ret+leaf+b-key" } */
/* { dg-require-effective-target lp64 } */

int foo (int);
int bar (int, int);

int __attribute__ ((target ("arch=armv8.3-a")))
func1 (int a, int b, int c)
{
  /* pacibsp */
  return a + foo (b) + c;
  /* retab */
}

/* eh_return.  */
void __attribute__ ((target ("arch=armv8.3-a")))
func4 (long offset, void *handler, int *ptr, int imm1, int imm2)
{
  /* pacibsp */
  *ptr = imm1 + foo (imm1) + imm2;
  if (handler)
    /* br */
    __builtin_eh_return (offset, handler);
  /* retab */
  return;
}

/* { dg-final { scan-assembler-times "pacibsp" 2 } } */
/* { dg-final { scan-assembler-times "retab" 2 } } */
