/* { dg-do run } */
register int g asm ("$18");

void __attribute__((noinline))
test (void)
{
  g = g + 1;
}

int
main (void)
{
  g = 2;
  test ();
  return g != 3;
}
