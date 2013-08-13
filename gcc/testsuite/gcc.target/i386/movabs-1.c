/* { dg-do assemble } */
/* { dg-options "-O2 -masm=intel" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target masm_intel } */

void
foo (void)
{
  *(volatile long*)0xFFFF800000000000 = -1;
}
