/* PR c/49706 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses" } */

#ifndef __cplusplus
#define bool _Bool
#endif
enum E { A, B };
bool b;
extern enum E foo_e (void);
extern bool foo_b (void);
extern int foo_i (void);

#ifdef __cplusplus
template <class T, class U> bool tfn1(T t, U u) { return (!t == u); } /* { dg-warning "logical not is only applied to the left hand side of comparison" "" { target c++ } } */
template <class T, class U> bool tfn2(T t, U u) { return ((!t) == u); }
template <class T, class U> bool tfn3(T t, U u) { return (!g(t) == u); } /* { dg-warning "logical not is only applied to the left hand side of comparison" "" { target c++ } } */
template <class T, class U> bool tfn4(T t, U u) { return ((!g(t)) == u); }
template <class T, class U> bool tfn5(T t, U u) { return (!!t == u); } /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
template <class T, class U> bool tfn6(T t, U u) { return (!!g(t) == u); } /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
template <int N> bool tfn7(int i1, int i2) { return (!i1 == i2); } /* { dg-warning "logical not is only applied to the left hand side of comparison" "" { target c++ } } */
#endif

void
fn1 (int i1, int i2, bool b1, bool b2)
{
  b = !i1 == i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 != i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 < i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 > i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 <= i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 >= i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  b = i1 == i2;
  b = i1 != i2;
  b = i1 < i2;
  b = i1 > i2;
  b = i1 <= i2;
  b = i1 >= i2;

  /* Parens suppress the warning.  */
  b = (!i1) == i2;
  b = (!i1) != i2;
  b = (!i1) < i2;
  b = (!i1) > i2;
  b = (!i1) <= i2;
  b = (!i1) >= i2;

  /* ...but not these parens.  */
  b = (!i1 == i2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 != i2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 < i2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 > i2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 <= i2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 >= i2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  b = !b1 == b2;
  b = !b1 != b2;
  b = !b1 < b2;
  b = !b1 > b2;
  b = !b1 <= b2;
  b = !b1 >= b2;

  b = !b1 == i2;
  b = !b1 != i2;
  b = !b1 < i2;
  b = !b1 > i2;
  b = !b1 <= i2;
  b = !b1 >= i2;

  b = !foo_i () == i1; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!foo_i ()) == i1;
  b = !foo_b () == b1;

  b = !!i1 == i2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 != i2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 < i2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 > i2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 <= i2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 >= i2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!foo_i () == i1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */

  b = !!b1 == i2;
  b = !!b1 != i2;
  b = !!b1 < i2;
  b = !!b1 > i2;
  b = !!b1 <= i2;
  b = !!b1 >= i2;

  /* Be careful here.  */
  b = (i1 == 0) != 0;
  b = (i1 == 0) == 0;
  b = (i1 != 0) != 0;
  b = (i1 != 0) == 0;

  b = !5 == 4; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!5 == 4; /* { dg-bogus "logical not is only applied to the left hand side of comparison" "" { xfail *-*-* } } */
  b = !1 == 1; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!1 == 1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" "" { xfail *-*-* } } */
}

void
fn2 (enum E e)
{
  b = e == B;
  b = e == foo_e ();
  b = foo_e () == A;
  b = foo_e () == foo_e ();

  b = !e == A;
  b = !e == foo_e (); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !foo_e () == A;
  b = !foo_e () == foo_e (); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  b = !(e == A);
  b = !(e == foo_e ());
  b = !(foo_e () == A);
  b = !(foo_e () == foo_e ());

  b = (!e) == A;
  b = (!e) == foo_e ();
  b = (!foo_e ()) == A;
  b = (!foo_e ()) == foo_e ();
}

void
fn3 (int i1, float f2)
{
  b = !i1 == f2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 != f2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 < f2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 > f2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 <= f2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !i1 >= f2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  b = i1 == f2;
  b = i1 != f2;
  b = i1 < f2;
  b = i1 > f2;
  b = i1 <= f2;
  b = i1 >= f2;

  /* Parens suppress the warning.  */
  b = (!i1) == f2;
  b = (!i1) != f2;
  b = (!i1) < f2;
  b = (!i1) > f2;
  b = (!i1) <= f2;
  b = (!i1) >= f2;

  /* ...but not these parens.  */
  b = (!i1 == f2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 != f2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 < f2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 > f2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 <= f2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!i1 >= f2); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  b = !!i1 == f2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 != f2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 < f2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 > f2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 <= f2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 >= f2; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
}

void
fn4 (enum E e)
{
  b = e == A;
  b = e == foo_e ();
  b = foo_e () == B;
  b = foo_e () == foo_e ();

  b = !e == B; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !e == foo_e (); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !foo_e () == B; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !foo_e () == foo_e (); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  b = !(e == B);
  b = !(e == foo_e ());
  b = !(foo_e () == B);
  b = !(foo_e () == foo_e ());

  b = (!e) == B;
  b = (!e) == foo_e ();
  b = (!foo_e ()) == B;
  b = (!foo_e ()) == foo_e ();
}
