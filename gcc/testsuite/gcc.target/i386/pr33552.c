/* PR rtl-optimization/33552 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

void
__attribute__((noinline))
foo (unsigned long *wp, unsigned long *up, long un, unsigned long *vp)
{
  long j;
  unsigned long prod_low, prod_high;
  unsigned long cy_dig;
  unsigned long v_limb;
  v_limb = vp[0];
  cy_dig = 64;
  for (j = un; j > 0; j--)
    {
      unsigned long u_limb, w_limb;
      u_limb = *up++;
      __asm__ (""
               : "=r" (prod_low), "=r" (prod_high)
               : "0" (u_limb), "1" (v_limb));
      __asm__ ("mov %5, %1; add %5, %0"
               : "=r" (cy_dig), "=&r" (w_limb)
               : "0" (prod_high), "rm" (0), "1" (prod_low), "rm" (cy_dig));
      *wp++ = w_limb;
    }
}

int
main (void)
{
  unsigned long wp[4];
  unsigned long up[4] = { 0x1248, 0x248a, 0x1745, 0x1853 };
  unsigned long vp = 0xdead;
  foo (wp, up, 4, &vp);
  if (wp[0] != 0x40 || wp[1] != 0xdeed || wp[2] != 0x1bd9a || wp[3] != 0x29c47)
    abort ();
  return 0;
}
