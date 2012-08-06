/* { dg-skip-if "" { pdp11-*-* } { "*" } { "" } } */
/* { dg-skip-if "Too many registers needed on 16-bit targets" { "m32c-*-*" "rl78-*-*" "xstormy16-*-*" } { "*" } { "" } } */
/* PR target/35318 */

void
foo ()
{
  double x = 4, y;
  __asm__ volatile ("" : "=r,r" (x), "=r,r" (y) : "%0,0" (x), "m,r" (8));
}
