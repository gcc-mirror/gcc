/* N3457 - The __COUNTER__ predefined macro */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic-errors" } */

#ifndef __COUNTER__
#error "__COUNTER__ not defined"
#endif

#define A(X) X + X
static_assert (A(__COUNTER__) == 0);
#define B(X)
B(__COUNTER__)
static_assert (__COUNTER__ == 1);
#define C(...) __VA_OPT__()
C(__COUNTER__)
static_assert (__COUNTER__ == 3);
#define D(...) __VA_OPT__(a)
int D(__COUNTER__) = 1;
static_assert (__COUNTER__ == 5);
#define E(X) #X
const char *b = E(__COUNTER__);
#define F(X) a##X
int F(__COUNTER__) = 2;
static_assert (__COUNTER__ == 6);
#define G(X) b##X = X
int G(__COUNTER__);
static_assert (__COUNTER__ == 8);
#if !defined(__COUNTER__) || (__COUNTER__ + 1 != __COUNTER__ + 0)
#error "Unexpected __COUNTER__ behavior")
#endif
static_assert (__COUNTER__ == 11);

extern int strcmp (const char *, const char *);
extern void abort ();

int
main ()
{
  if (a != 1
      || strcmp (b, "__COUNTER__")
      || a__COUNTER__ != 2
      || b__COUNTER__ != 7)
    abort ();
}
