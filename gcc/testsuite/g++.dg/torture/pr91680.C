/* PR middle-end/91680 */
/* { dg-do run { target { ilp32 || lp64 } } } */

extern "C" void abort ();

#include "../../gcc.dg/tree-ssa/pr91680.c"

int
main ()
{
  unsigned char i;
  for (i = 0; i < __SIZEOF_INT__ * __CHAR_BIT__; i++)
    {
      volatile unsigned long long q = 1 << i;
      if (foo (i) != 256 / q)
	abort ();
      q = 1U << i;
      if (bar (i) != 256 / q)
	abort ();
      q = 1 << i;
      if (baz (i, (1U << i) - 1) != ((1U << i) - 1) / q)
	abort ();
      if (baz (i, 1U << i) != (1U << i) / q)
	abort ();
      if (baz (i, -1) != -1 / q)
	abort ();
      q = 1U << i;
      if (qux (i, (1U << i) - 1) != ((1U << i) - 1) / q)
	abort ();
      if (qux (i, 1U << i) != (1U << i) / q)
	abort ();
      if (qux (i, -1) != -1 / q)
	abort ();
    }
}
