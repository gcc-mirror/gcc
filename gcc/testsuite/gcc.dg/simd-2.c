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
  b = c; /* { dg-error "incompatible types in assignment" } */
  d = a; /* { dg-error "incompatible types in assignment" } */

  /* Casting between SIMDs of the same size.  */
  e = (typeof (e)) a;

  /* Assignment between scalar and SIMD of different size.  */
  foo = a; /* { dg-error "incompatible types in assignment" } */

  /* Casted assignment between scalar and SIMD of same size.  */
  foo = (typeof (foo)) foo2; /* { dg-bogus "aggregate value used where a float was expected" "" { xfail *-*-* } } */

  /* Casted assignment between scalar and SIMD of different size.  */
/*  foo1 = (typeof (foo1)) foo2;  { dg*error "can't convert between vector values of different size" } */
  foo1 = (typeof (foo1)) foo2; /* { dg-bogus "aggregate value used where a float was expected" "" { xfail *-*-* } } */

  /* Operators on compatible SIMD types.  */
  a += b + b;
  a -= b;
  a *= b;
  a /= b;
  a = +b;
  c = -d;

  /* Operators on incompatible SIMD types.  */
  a = b + c; /* { dg-error "can't convert between vector values of different size" } */
  a = b - c; /* { dg-error "can't convert between vector values of different size" } */
  a = b * c; /* { dg-error "can't convert between vector values of different size" } */
  a = b / c; /* { dg-error "can't convert between vector values of different size" } */
}
