/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } { "*" } { "" } } */
/* { dg-lto-options { { -O2 -flto -w } } } */

register int r asm("esi");

void foo(void)
{
  if (r)
    __asm__("sar\t%0" : "+r" (r));
  __asm__("sar\t%0" : "+r" (r));
}
