/* { dg-do run { target int128 } } */

typedef unsigned __int128 u128;

__attribute__((noipa)) u128
f (u128 x)
{
  return ((x << 64) ^ (u128) 1) | (x >> 64);
}

__attribute__((noipa)) u128
g (u128 x)
{
  return ((x << 96) ^ ((u128) 1 << 32)) | (x >> 32);
}

int
main (void)
{
  u128 x = (u128) 1 << 64;
  if (f (x) != 1)
    __builtin_abort ();
  if (g (x) != ((u128) 1 << 32))
    __builtin_abort ();
  return 0;
}
