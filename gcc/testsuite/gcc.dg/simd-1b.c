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
  a %= b; /* { dg-bogus "invalid operands to binary %" "" { xfail *-*-* } } */
  c &= d;
  a |= b;
  c ^= d;
  a >>= b; /* { dg-bogus "invalid operands to binary >>" "" { xfail *-*-* } } */
  c <<= d; /* { dg-bogus "invalid operands to binary <<" "" { xfail *-*-* } } */
  a = +b;
  c = ~d;

  /* Operators on incompatible SIMD types.  */
/*  a = b % c;  { dg*error "can't convert between vector values of different size" } */
  a = b % c; /* { dg-bogus "invalid operands to binary %" "" { xfail *-*-* } } */
  d = c & b; /* { dg-error "can't convert between vector values of different size" } */
  a = b | c; /* { dg-error "can't convert between vector values of different size" } */
  d = c ^ b; /* { dg-error "can't convert between vector values of different size" } */
/*  a = b >> c;  { dg*error "can't convert between vector values of different size" } */
  a = b >> c; /* { dg-bogus "invalid operands to binary >>" "" { xfail *-*-* } } */
/*  d = c << b;  { dg*error "can't convert between vector values of different size" } */
  d = c << b; /* { dg-bogus "invalid operands to binary <<" "" { xfail *-*-* } } */
}
