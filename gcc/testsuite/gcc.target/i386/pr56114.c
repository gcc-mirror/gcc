/* { dg-do assemble } */
/* { dg-options "-O2 -masm=intel" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target masm_intel } */

long
foo2 (void)
{
  return *(volatile int *) 0xFEE00000;
}
