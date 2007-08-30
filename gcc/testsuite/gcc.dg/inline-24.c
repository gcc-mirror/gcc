/* Verify that gnu_inline inlines disregard inlining limits.  */
/* { dg-do link } */
/* { dg-options "-O2" } */

extern int foo (int);
extern int baz (int);

extern inline __attribute__((gnu_inline))
int foo (int x)
{
  int i;
  if (!__builtin_constant_p (x))
    {
#define B(n) baz (1##n) + baz (2##n) + baz (3##n) \
	     + baz (4##n) + baz (5##n) + baz (6##n)
#define C(n) B(1##n) + B(2##n) + B(3##n) + B(4##n) + B(5##n) + B(6##n)
#define D(n) C(1##n) + C(2##n) + C(3##n) + C(4##n) + C(5##n) + C(6##n)
      return D(0) + D(1) + D(2) + D(3) + D(4)
	     + D(5) + D(6) + D(7) + D(8) + D(9);
    }
  return 0;
}

int
main (void)
{
  return foo (0);
}
