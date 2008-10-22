/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* Origin: Aldy Hernandez <aldyh@redhat.com>.  */
/* Purpose: Program to test generic SIMD support.  */

typedef int __attribute__((vector_size (16))) v4si;
typedef short __attribute__((vector_size (16))) v8hi;
typedef int __attribute__((vector_size (8))) v2si;
typedef unsigned int __attribute__((vector_size (16))) uv4si;

v4si a, b;
v2si c, d;
v8hi e;
uv4si f;

long long foo;
int foo1;
short foo2 __attribute__((vector_size (8)));

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

  /* Different signed SIMD assignment.  */
  f = a; /* { dg-message "note: use -flax-vector-conversions to permit conversions between vectors with differing element types or numbers of subparts" } */
  /* { dg-error "incompatible types when assigning" "" { target *-*-* } 35 } */

  /* Casted different signed SIMD assignment.  */
  f = (uv4si) a;

  /* Assignment between scalar and SIMD of different size.  */
  foo = a; /* { dg-error "incompatible types when assigning" } */

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
  a = b + c; /* { dg-error "invalid operands to binary +" } */
  a = b - c; /* { dg-error "invalid operands to binary -" } */
  a = b * c; /* { dg-error "invalid operands to binary *" } */
  a = b / c; /* { dg-error "invalid operands to binary /" } */
}
