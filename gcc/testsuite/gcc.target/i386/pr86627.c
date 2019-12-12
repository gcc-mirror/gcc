/* PR middle-end/86627 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "call\[^\n\r]*__divti3" } } */

__int128_t
f1 (__int128_t a)
{
  return a / 2;
}

__int128_t
f2 (__int128_t a)
{
  return a / -2;
}

__int128_t
f3 (__int128_t a)
{
  return a / 0x4000000000000000LL;
}

__int128_t
f4 (__int128_t a)
{
  return a / -0x4000000000000000LL;
}
