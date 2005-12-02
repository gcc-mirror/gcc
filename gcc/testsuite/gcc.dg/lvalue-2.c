/* Test diagnostic messages for invalid lvalues and non-modifiable
   lvalues.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int a, b;

void
f0 (void)
{
  (a+b) = 1; /* { dg-error "error: lvalue required as left operand of assignment" } */
  (a+b)++; /* { dg-error "error: lvalue required as increment operand" } */
  ++(a+b); /* { dg-error "error: lvalue required as increment operand" } */
  (a+b)--; /* { dg-error "error: lvalue required as decrement operand" } */
  --(a+b); /* { dg-error "error: lvalue required as decrement operand" } */
  &(a+b); /* { dg-error "error: lvalue required as unary '&' operand" } */
}

const int c;
const struct { int x; } d;
struct { const int x; } e;
const int *f;

void
f1 (void)
{
  c = 1; /* { dg-error "error: assignment of read-only variable 'c'" } */
  d.x = 1; /* { dg-error "error: assignment of read-only variable 'd'" } */
  e.x = 1; /* { dg-error "error: assignment of read-only member 'x'" } */
  *f = 1; /* { dg-error "error: assignment of read-only location" } */
  c++; /* { dg-error "error: increment of read-only variable 'c'" } */
  d.x++; /* { dg-error "error: increment of read-only variable 'd'" } */
  e.x++; /* { dg-error "error: increment of read-only member 'x'" } */
  (*f)++; /* { dg-error "error: increment of read-only location" } */
  ++c; /* { dg-error "error: increment of read-only variable 'c'" } */
  ++d.x; /* { dg-error "error: increment of read-only variable 'd'" } */
  ++e.x; /* { dg-error "error: increment of read-only member 'x'" } */
  ++(*f); /* { dg-error "error: increment of read-only location" } */
  c--; /* { dg-error "error: decrement of read-only variable 'c'" } */
  d.x--; /* { dg-error "error: decrement of read-only variable 'd'" } */
  e.x--; /* { dg-error "error: decrement of read-only member 'x'" } */
  (*f)--; /* { dg-error "error: decrement of read-only location" } */
  --c; /* { dg-error "error: decrement of read-only variable 'c'" } */
  --d.x; /* { dg-error "error: decrement of read-only variable 'd'" } */
  --e.x; /* { dg-error "error: decrement of read-only member 'x'" } */
  --(*f); /* { dg-error "error: decrement of read-only location" } */
}
