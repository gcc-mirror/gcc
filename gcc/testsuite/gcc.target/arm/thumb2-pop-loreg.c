/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-Os" } */

int __attribute__((noinline)) f (void)
{
  asm ("");
}

int g (void)
{
  char buf[32];
  register char *x asm ("r4") = buf;
  asm volatile ("" : : "r" (x));
  return f();
}
/* Unstacking a single low register in thumb2 should use POP.  */
/* { dg-final { scan-assembler "pop\t{r4}" } } */
