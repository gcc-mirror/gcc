/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1150 6: Constants.
   C99 6.4.4.2: Floating constants.

   The df, dd, dl, DF, DD and DL suffixes shall not be used in a
   hexadecimal-floating-constant.  */

_Decimal32 bad1 = 0x1.0p1df;   /* { dg-error "invalid suffix" "hexadecimal floating constant" } */
_Decimal32 bad2 = 0x1.0p1DF;   /* { dg-error "invalid suffix" "hexadecimal floating constant" } */
_Decimal64 bad3 = 0x2.0p-2dd;  /* { dg-error "invalid suffix" "hexadecimal floating constant" } */
_Decimal64 bad4 = 0x2.0p2DD;   /* { dg-error "invalid suffix" "hexadecimal floating constant" } */
_Decimal128 bad5 = 0x3.0p3dl;  /* { dg-error "invalid suffix" "hexadecimal floating constant" } */
_Decimal128 bad6 = 0x3.0p3DL;  /* { dg-error "invalid suffix" "hexadecimal floating constant" } */
