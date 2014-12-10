/* PR tree-optimization/59920 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-require-effective-target nonlocal_goto } */

void *bar (void **);
void *baz (int, void **);

#define A(n) __label__ l##n;
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) \
	     A(n##5) A(n##6) A(n##7) A(n##8) A(n##9)
#define C(n) B(n##0) B(n##1) B(n##2) B(n##3) B(n##4) \
	     B(n##5) B(n##6) B(n##7) B(n##8) B(n##9)
#define D C(1)

int
foo (void)
{
  D
  int bar (int i)
  {
    switch (i)
      {
#undef A
#define A(n) \
      case n: goto l##n;
      D
      }
    return i;
  }
  int w = 0;
#undef A
#define A(n) int w##n = 0;
  D
#undef A
#define A(n) \
  { l##n:; 				\
    w##n += bar (10000 + n) - 10000;	\
    w##n += bar (10001 + n) - 10000;	\
    bar (n + 1);			\
    return w##n;			\
  }
  D
#undef A
#define A(n) w += w##n;
  D
  return w;
}
