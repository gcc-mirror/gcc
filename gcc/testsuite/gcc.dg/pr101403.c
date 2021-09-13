/* { dg-do run } */
/* { dg-options "-O2" } */
unsigned int foo (unsigned int a)
{
  unsigned int u = 0;
  unsigned short b = __builtin_bswap16 (a);
  return b >> (u, 12);
}

int main (void)
{
  unsigned int x = foo (0x80);
  if (x != 0x0008)
    __builtin_abort ();
  return 0;
}

