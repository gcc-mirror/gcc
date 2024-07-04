/* PR middle-end/114628 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -g" } */

int foo (int);
#if __BITINT_MAXWIDTH__ >= 129
__attribute__((returns_twice)) int bar (_BitInt(129) x);

void
baz (int x, _BitInt(129) y)
{
  void *q[] = { &&l1, &&l2 };
l2:
  x = foo (foo (3));
  bar (y);
  goto *q[x & 1];
l1:;
}

void
qux (int x, _BitInt(129) y)
{
  void *q[] = { &&l1, &&l2 };
l2:
  x = foo (foo (3));
  bar (y);
l1:;
}
#endif
