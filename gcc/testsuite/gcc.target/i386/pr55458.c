/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-fPIC" } */

int a, b, c;

void
foo (void)
{
  asm volatile ("":"+m" (a), "+m" (b), "+m" (c)); /* { dg-error "operand has impossible constraints" } */
}
