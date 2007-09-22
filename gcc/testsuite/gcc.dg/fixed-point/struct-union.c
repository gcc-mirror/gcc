/* { dg-do compile } */
/* { dg-options "-O -Wall" } */

/* C99 6.5.2.3 Structure and union members.
   If the first expression has qualified type, the result has the so-qualified
   version of the type of the designated member.
   Based on the test from ../dfp/.  */

struct s {_Fract f; const long _Fract lf;};
struct sv { volatile _Fract f; volatile long _Fract lf; };
union u
{
  const long _Fract lf;
  _Fract f;
  const struct s cs;
};

struct s s;
struct sv sv;
const struct s cs;

union u u;
const union u cu;

struct s g (struct s s)
{
  return s;
}

union u h (union u u)
{
  return u;
}

void f()
{
  cs.f = 0.1r; /* { dg-error "assignment of read-only variable" } */
  cs.lf = 0.2lr; /* { dg-error "assignment of read-only variable" } */
  s.lf = 0.3lr;  /* { dg-error "assignment of read-only member" } */

  s.f = 0.4r;
  u.f = 0.5r;

  u.lf = 0.6lr;    /* { dg-error "assignment of read-only member" } */
  u.cs.f = 0.7r; /* { dg-error "assignment of read-only member" } */
  u.cs.lf = 0.8lr; /* { dg-error "assignment of read-only member" } */

  cu.f = 0.9r;   /* { dg-error "assignment of read-only variable" } */

  cu.lf = 0.01lr;    /* { dg-error "assignment of read-only variable" } */
  cu.cs.f = 0.02r; /* { dg-error "assignment of read-only variable" } */
  cu.cs.lf = 0.03lr; /* { dg-error "assignment of read-only variable" } */

  /* f().x is a valid postfix expression but is not an lvalue if
     function f() returning a structure or union.  */
  g(s).f = 0.04r;  /* { dg-error "lvalue required" } */
  h(u).lf = 0.05lr;  /* { dg-error "lvalue required" } */

  /* Test assignment to volatile structure members.  */
  sv.f = 0.06r;
  sv.lf = 0.07lr;
}

