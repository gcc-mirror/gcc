/* PR middle-end/98099 */
/* Reported by G. Steinmetz <gscfq@t-online.de> */

/* { dg-do compile { target dfp } } */
/* { dg-options "-fsso-struct=big-endian" } */

struct S { _Decimal128 a; };

_Decimal128 f (struct S x)
{
  return x.a; /* { dg-message "sorry, unimplemented: reverse storage order" "" { target { ! int128 } } } */
}
