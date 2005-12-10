/* PR rtl-optimization/19296 */
/* Origin: Falk Hueffner <falk@debian.org> */
/* Testcase by Andrew Pinski <pinskia@gcc.gnu.org> */

/* { dg-do run } */
/* { dg-options "-O" } */
/* { dg-options "-O -mtune=i686" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

extern void abort();

void f(unsigned short ad)
{
  if ((short) (ad - 0x4000) >= 0)
    abort();
}

int main(void)
{
  f(0xc000);
  return 0;
}
