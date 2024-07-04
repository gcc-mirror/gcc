/* Test DFP types and constants rejected if no DFP support.  Bug
   91985.  */
/* { dg-do compile { target { ! dfp } } } */
/* { dg-options "-std=c23" } */

_Decimal32 d32a; /* { dg-error "not supported" } */
_Decimal64 d64a; /* { dg-error "not supported" } */
_Decimal128 d128a; /* { dg-error "not supported" } */

_Bool d32b = 1.0DF; /* { dg-error "not supported" } */
_Bool d64b = 1.0DD; /* { dg-error "not supported" } */
_Bool d128b = 1.0DL; /* { dg-error "not supported" } */
