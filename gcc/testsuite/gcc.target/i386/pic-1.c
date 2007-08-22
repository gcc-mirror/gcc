/* PR target/8340 */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC" } */

int foo ()
{
  static int a;

  __asm__ __volatile__ (  /* { dg-error "PIC register" } */
    "xorl %%ebx, %%ebx\n"
    "movl %%ebx, %0\n"
    : "=m" (a)
    :
    : "%ebx"
  );

  return a;
}
