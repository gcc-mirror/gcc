/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* Origin: Aldy Hernandez <aldyh@redhat.com>.  */
/* Purpose: Program to test generic SIMD support.  */

typedef int __attribute__((vector_size (16))) v4si;
typedef int __attribute__((vector_size (8))) v2si;

v4si a, b;
v2si c, d;

void
hanneke ()
{
  /* Operators on compatible SIMD types.  */
  a %= b;
  c &= d;
  a |= b;
  c ^= d;
  a >>= b; /* { dg-error "invalid operands to binary >>" } */
  c <<= d; /* { dg-error "invalid operands to binary <<" } */
  a = +b;
  c = ~d;

  /* Operators on incompatible SIMD types.  */
  a = b % c; /* { dg-error "invalid operands to binary" } */
  a = b % c; /* { dg-error "invalid operands to binary" } */
  d = c & b; /* { dg-error "invalid operands to binary" } */
  a = b | c; /* { dg-error "invalid operands to binary" } */
  d = c ^ b; /* { dg-error "invalid operands to binary" } */
  a = b >> c; /*  { dg-error "invalid operands to binary" } */
  a = b >> c; /* { dg-error "invalid operands to binary" } */
  d = c << b; /* { dg-error "invalid operands to binary" } */
  d = c << b; /* { dg-error "invalid operands to binary" } */
}
