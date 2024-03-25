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

/* eh_return.  */
void __attribute__ ((target ("arch=armv8.3-a")))
func4 (long offset, void *handler, int *ptr, int imm1, int imm2)
{
  /* paciasp */
  *ptr = imm1 + foo (imm1) + imm2;
  if (handler)
    /* br */
    __builtin_eh_return (offset, handler);
  /* retaa */
  return;
}

/* { dg-final { scan-assembler-times "paciasp" 2 } } */
/* { dg-final { scan-assembler-times "retaa" 2 } } */
