/* PR target/81979 */
/* { dg-do link } */
/* { dg-options "-O2 -w" } */
/* { dg-additional-options "-fPIC" { target fpic } } */
/* { dg-additional-options "-freorder-blocks-and-partition" { target freorder } } */

int d;

__attribute__((noinline, noclone)) void
foo (int x)
{
  int c;
  while (c < 1)
    {
      int o;
      for (o = 0; o < 4; ++o)
	c /= (x != 0) ? 2 : x;
    }

  d = 1;
  for (;;)
    ;
}

int
main ()
{
  asm volatile ("" : : "r" (&d) : "memory");
  foo (d);
  asm volatile ("" : : "r" (&d) : "memory");
  return 0;
}
