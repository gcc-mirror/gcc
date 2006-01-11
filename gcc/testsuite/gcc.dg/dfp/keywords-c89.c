/* { dg-do compile } */
/* { dg-options "-std=c89" } */

/* Decimal float keywords are not recognized in C89 mode.  */

_Decimal32 x;		/* { dg-error "error" } */
_Decimal64 y;		/* { dg-error "error" } */
_Decimal128 z;		/* { dg-error "error" } */
