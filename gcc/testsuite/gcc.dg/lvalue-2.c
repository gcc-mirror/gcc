/* Test diagnostic messages for invalid lvalues and non-modifiable
   lvalues.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

int a, b;

void
f0 (void)
{
  (a+b) = 1; /* { dg-error "lvalue required as left operand of assignment" } */
  (a+b)++; /* { dg-error "lvalue required as increment operand" } */
  ++(a+b); /* { dg-error "lvalue required as increment operand" } */
  (a+b)--; /* { dg-error "lvalue required as decrement operand" } */
  --(a+b); /* { dg-error "lvalue required as decrement operand" } */
  &(a+b); /* { dg-error "lvalue required as unary '&' operand" } */
}

const int c;
const struct { int x; } d;
struct { const int x; } e;
const int *f;

void
f1 (void)
{
  c = 1; /* { dg-error "assignment of read-only variable 'c'" } */
  d.x = 1; /* { dg-error "assignment of member 'x' in read-only object" } */
  e.x = 1; /* { dg-error "assignment of read-only member 'x'" } */
  *f = 1; /* { dg-error "assignment of read-only location" } */
  c++; /* { dg-error "increment of read-only variable 'c'" } */
  d.x++; /* { dg-error "increment of member 'x' in read-only object" } */
  e.x++; /* { dg-error "increment of read-only member 'x'" } */
  (*f)++; /* { dg-error "increment of read-only location" } */
  ++c; /* { dg-error "increment of read-only variable 'c'" } */
  ++d.x; /* { dg-error "increment of member 'x' in read-only object" } */
  ++e.x; /* { dg-error "increment of read-only member 'x'" } */
  ++(*f); /* { dg-error "increment of read-only location" } */
  c--; /* { dg-error "decrement of read-only variable 'c'" } */
  d.x--; /* { dg-error "decrement of member 'x' in read-only object" } */
  e.x--; /* { dg-error "decrement of read-only member 'x'" } */
  (*f)--; /* { dg-error "decrement of read-only location" } */
  --c; /* { dg-error "decrement of read-only variable 'c'" } */
  --d.x; /* { dg-error "decrement of member 'x' in read-only object" } */
  --e.x; /* { dg-error "decrement of read-only member 'x'" } */
  --(*f); /* { dg-error "decrement of read-only location" } */
}
