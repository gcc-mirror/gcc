/* PR rtl-optimization/79985 */
/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O3 -fschedule-insns -fselective-scheduling" } */

long a;
int b;
void
c ()
{
  __asm("" : "=r"(a) : "0"(c));
  __asm("" : "=r"(b));
}
