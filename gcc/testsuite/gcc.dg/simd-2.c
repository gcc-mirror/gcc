/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* Origin: Aldy Hernandez <aldyh@redhat.com>.  */
/* Purpose: Program to test generic SIMD support.  */

typedef float __attribute__((vector_size(8))) v2sf;
typedef float __attribute__((vector_size(16))) v4sf;
typedef double __attribute__((vector_size(16))) v2df;

v4sf a, b;
v2sf c, d;
v2df e;

double foo;
float foo1;
v2sf foo2;

void
hanneke ()
{
  /* Assignment.  */
  a = b;

  /* Assignment of different types.  */
  b = c; /* { dg-error "incompatible types when assigning" } */
  d = a; /* { dg-error "incompatible types when assigning" } */

  /* Casting between SIMDs of the same size.  */
  e = (typeof (e)) a;

  /* Assignment between scalar and SIMD of different size.  */
  foo = a; /* { dg-error "incompatible types when assigning" } */

  /* Casted assignment between scalar and SIMD of same size.  */
  foo = (typeof (foo)) foo2; /* { dg-error "vector value used where a floating-point was expected" } */

  /* Casted assignment between scalar and SIMD of different size.  */
  foo1 = (typeof (foo1)) foo2; /* { dg-error "vector value used where a floating-point was expected" } */

  /* Operators on compatible SIMD types.  */
  a += b + b;
  a -= b;
  a *= b;
  a /= b;
  a = +b;
  c = -d;

  /* Operators on incompatible SIMD types.  */
  a = b + c; /* { dg-error "invalid operands to binary" } */
  a = b - c; /* { dg-error "invalid operands to binary" } */
  a = b * c; /* { dg-error "invalid operands to binary" } */
  a = b / c; /* { dg-error "invalid operands to binary" } */
}
