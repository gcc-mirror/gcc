/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* Origin: Aldy Hernandez <aldyh@redhat.com>.  */
/* Purpose: Program to test generic SIMD support.  */

typedef int __attribute__((mode(V4SI))) v4si;
typedef int __attribute__((mode(V8HI))) v8hi;
typedef int __attribute__((mode(V2SI))) v2si;
typedef unsigned int __attribute__((mode(V4SI))) uv4si;

v4si a, b;
v2si c, d;
v8hi e;
uv4si f;

int foo __attribute__((mode(DI)));
int foo1 __attribute__((mode(SI)));
int foo2 __attribute__((mode(V4HI)));

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

  /* Different signed SIMD assignment.  */
  f = a; /* { dg-error "incompatible types in assignment" } */

  /* Casted different signed SIMD assignment.  */
  f = (uv4si) a;

  /* Assignment between scalar and SIMD of different size.  */
  foo = a; /* { dg-error "incompatible types in assignment" } */

  /* Casted assignment between scalar and SIMD of same size.  */
  foo = (typeof (foo)) foo2;

  /* Casted assignment between scalar and SIMD of different size.  */
  foo1 = (typeof (foo1)) foo2; /* { dg-error "can't convert between vector values of different size" } */

  /* Operators on compatible SIMD types.  */
  a += b + b;
  a -= b;
  a *= b;
  a /= b;
  a = -b;

  /* Operators on incompatible SIMD types.  */
  a = b + c; /* { dg-error "can't convert between vector values of different size" } */
  a = b - c; /* { dg-error "can't convert between vector values of different size" } */
  a = b * c; /* { dg-error "can't convert between vector values of different size" } */
  a = b / c; /* { dg-error "can't convert between vector values of different size" } */
}
