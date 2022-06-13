/* PR target/105911 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 v, x;
unsigned __int128 w;

void bar (__int128, __int128);

void
foo (void)
{
  bar (v /= v, v >> (v &= 0x100000001));
  bar (w /= w, w >> (w &= 0x300000003));
  bar (x /= x, x << (x &= 0x700000007));
}
