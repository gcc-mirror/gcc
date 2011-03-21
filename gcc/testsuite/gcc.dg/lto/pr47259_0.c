/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { x86_64-*-* } } { "*" } { "" } } */
/* { dg-lto-options { { -O2 -flto -w } } } */
/* { dg-require-effective-target lp64 } */

register int r asm("esi");

void foo(void)
{
  if (r)
    __asm__("sar\t%0" : "+r" (r));
  __asm__("sar\t%0" : "+r" (r));
}
