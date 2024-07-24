/* { dg-do run { target int128 } } */

char a[16];
unsigned short u;

__int128
foo (int i)
{
  i -= (unsigned short) ~u;
  a[(unsigned short) i] = 1;
  return i;
}

int
main ()
{
  __int128 x = foo (0);
  if (x != -0xffff)
    __builtin_abort();
}
