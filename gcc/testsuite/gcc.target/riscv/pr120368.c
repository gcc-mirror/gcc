/* { dg-do run } */

int g;

int
foo (int s, int v)
{
  __builtin_memset (&g, v >> (s & 31), sizeof(g));
  return g;
}

int
main ()
{
  int x = foo (-16, 0xdffff);
  if (x != 0x0d0d0d0d)
    __builtin_abort();
  __builtin_exit (0);
}
