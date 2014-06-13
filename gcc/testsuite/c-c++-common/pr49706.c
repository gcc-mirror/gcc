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
template <class T, class U> bool f1(T t, U u) { return (!t == u); } /* { dg-warning "logical not is only applied to the left hand side of comparison" "" { target c++ } 15 } */
template <class T, class U> bool f2(T t, U u) { return ((!t) == u); }
template <class T, class U> bool f3(T t, U u) { return (!g(t) == u); } /* { dg-warning "logical not is only applied to the left hand side of comparison" "" { target c++ } 17 } */
template <class T, class U> bool f4(T t, U u) { return ((!g(t)) == u); }
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

  b = !foo_i () == i1; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = (!foo_i ()) == i1;
  b = !foo_b () == b1;

  b = !!i1 == i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 != i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 < i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 > i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 <= i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!i1 >= i2; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !!foo_i () == i1; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */

  /* Be careful here.  */
  b = (i1 == 0) != 0;
  b = (i1 == 0) == 0;
  b = (i1 != 0) != 0;
  b = (i1 != 0) == 0;
}

void
fn2 (enum E e)
{
  b = e == B;
  b = e == foo_e ();
  b = foo_e () == A;
  b = foo_e () == foo_e ();

  b = !e == A; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !e == foo_e (); /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
  b = !foo_e () == A; /* { dg-warning "logical not is only applied to the left hand side of comparison" } */
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
