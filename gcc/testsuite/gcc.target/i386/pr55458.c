/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC" } */

/* Test verifies that %ebx is no longer fixed when generating PIC code on i686.  */

int a, b, c;

void
foo (void)
{
  asm volatile ("":"+m" (a), "+m" (b), "+m" (c));
}
