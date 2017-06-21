/* PR target/8340 */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-require-effective-target fpic } */
/* { dg-skip-if "No Windows PIC" { *-*-mingw* *-*-cygwin } } */
/* { dg-options "-fPIC" } */

/* Test verifies that %ebx is no longer fixed when generating PIC code on i686.  */

int foo ()
{
  static int a;

  __asm__ __volatile__ (
    "xorl %%ebx, %%ebx\n"
    "movl %%ebx, %0\n"
    : "=m" (a)
    :
    : "%ebx"
  );

  return a;
}
