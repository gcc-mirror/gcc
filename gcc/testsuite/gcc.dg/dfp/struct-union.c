/* { dg-do compile } */
/* { dg-options "-O -Wall" } */

/* C99 6.5.2.3 Structure and union members.
   If the first expression has qualified type, the result has the so-qualified 
   version of the type of the designated member.  */

struct s {_Decimal32 d32; const _Decimal64 d64;};
struct sv { volatile _Decimal32 d32; volatile _Decimal64 d64; };
union u 
{
  const _Decimal64 d64; 
  _Decimal32 d32; 
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
  cs.d32 = 1.23dd; /* { dg-error "assignment of read-only variable" } */
  cs.d64 = 1.23df; /* { dg-error "assignment of read-only variable" } */
  s.d64 = 1.23df;  /* { dg-error "assignment of read-only member" } */

  s.d32 = 1.23dd;
  u.d32 = 1.23dd;

  u.d64 = 1.23df;    /* { dg-error "assignment of read-only member" } */
  u.cs.d32 = 1.23dd; /* { dg-error "assignment of read-only member" } */
  u.cs.d64 = 1.23df; /* { dg-error "assignment of read-only member" } */
  
  cu.d32 = 1.23dd;   /* { dg-error "assignment of read-only variable" } */

  cu.d64 = 1.23df;    /* { dg-error "assignment of read-only variable" } */
  cu.cs.d32 = 1.23dd; /* { dg-error "assignment of read-only variable" } */
  cu.cs.d64 = 1.23df; /* { dg-error "assignment of read-only variable" } */

  /* f().x is a valid postfix expression but is not an lvalue if 
     function f() returning a structure or union.  */
  g(s).d32 = 1.23dd;  /* { dg-error "lvalue required" } */
  h(u).d64 = 1.23df;  /* { dg-error "lvalue required" } */

  /* Test assignment to volatile structure members.  */
  sv.d32 = 1.1df;
  sv.d64 = 1.1dd;
}

