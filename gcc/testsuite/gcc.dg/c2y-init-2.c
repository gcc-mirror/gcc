/* Test invalid initializers that are consistent with the syntax: undefined
   behavior ("shall" in Semantics not Constraints) before C2y, constraint
   violation in C2y.  Structure and union cases.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

struct s1 { int a, b; };
struct s2 { struct s1 x; };
struct s3 { struct s2 x; };
union u1 { int a; };
union u2 { union u1 x; };
union u3 { union u2 x; };

struct s1 s1v;
volatile struct s2 s2v;
union u1 u1v;
const union u2 u2v;

void
f ()
{
  struct s1 ts1a = {}, ts1b = s1v, ts1c = { 1, 2 };
  const struct s2 ts2a = {}, ts2b = s2v, ts2c = { s1v }, ts2d = { 1 };
  volatile struct s3 ts3a = { s2v }, ts3b = { s1v };
  union u1 tu1a = {}, tu1b = u1v, tu1c = { 1 };
  const union u2 tu2a = {}, tu2b = u2v, tu2c = { u1v }, tu2d = { 1 };
  volatile union u3 tu3a = { u2v }, tu3b = { u1v };
  struct s2 es2a = 1; /* { dg-error "invalid initializer" } */
  struct s2 es2b = s1v; /* { dg-error "invalid initializer" } */
  struct s1 es1a = s2v; /* { dg-error "invalid initializer" } */
  union u2 eu2a = u1v; /* { dg-error "invalid initializer" } */
  union u1 eu1a = 1; /* { dg-error "invalid initializer" } */
}
