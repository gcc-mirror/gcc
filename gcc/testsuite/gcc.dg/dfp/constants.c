/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1150 6: Constants.
   C99 6.4.4.2: Floating constants.  */

_Decimal32 a = 1.1df;
_Decimal32 b = -.003DF;
_Decimal64 c = 11e-1dl;
_Decimal64 d = -.3DL;
_Decimal128 e = 000.3e0dl;
_Decimal128 f = 3000300030003e0DL;
