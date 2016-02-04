/* PR target/69442 */
/* { dg-do run } */
/* { dg-options "-Og" } */

unsigned long long __attribute__ ((noinline, noclone))
foo (unsigned int x, unsigned long long y)
{
  x |= 0xffff;
  y -= 0xffULL;
  y %= 0xffff0000ffffffe7ULL;
  return x + y;
}

int
main ()
{
  if (sizeof (unsigned long long) * __CHAR_BIT__ != 64)
    return 0;

  if (foo (0, 0) != 0xffff0000ff19ULL)
    __builtin_abort ();
  return 0;
}
