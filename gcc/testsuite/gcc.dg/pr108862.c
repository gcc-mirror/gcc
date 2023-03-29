/* PR target/108862 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

unsigned long long a[2] = { 0x04a13945d898c296ULL, 0x0000100000000fffULL };
unsigned long long b[4] = { 0x04a13945d898c296ULL, 0, 0, 0x0000100000000fffULL };

__attribute__((noipa)) unsigned __int128
foo (int x, unsigned long long *y, unsigned long long *z)
{
  unsigned __int128 w = 0;
  for (int i = 0; i < x; i++)
    w += (unsigned __int128)*y++ * (unsigned __int128)*z--;
  return w;
}

int
main ()
{
  unsigned __int128 x = foo (1, &a[0], &a[1]);
  unsigned __int128 y = foo (2, &b[0], &b[3]);
  if ((unsigned long long) (x >> 64) != 0x0000004a13945dd3ULL
      || (unsigned long long) x != 0x9b1c8443b3909d6aULL
      || x != y)
    __builtin_abort ();
  return 0;
}
