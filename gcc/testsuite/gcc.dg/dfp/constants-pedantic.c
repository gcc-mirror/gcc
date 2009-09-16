/* { dg-do compile } */
/* { dg-options "-pedantic" } */

/* N1150 6: Constants.
   C99 6.4.4.2: Floating constants.  */

#include "dfp-dbg.h"

_Decimal32 a = 1.1df;		/* { dg-warning "GCC extension|ISO C" } */
_Decimal32 b = -.003DF;		/* { dg-warning "GCC extension|ISO C" } */
_Decimal64 c = 11e-1dl;		/* { dg-warning "GCC extension|ISO C" } */
_Decimal64 d = -.3DL;		/* { dg-warning "GCC extension|ISO C" } */
_Decimal128 e = 000.3e0dl;	/* { dg-warning "GCC extension|ISO C" } */
_Decimal128 f = 3000300030003e0DL;  /* { dg-warning "GCC extension|ISO C" } */
