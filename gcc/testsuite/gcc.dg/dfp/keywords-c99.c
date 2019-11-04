/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

/* Decimal float keywords are diagnosed in pedantic C99 mode.  */

_Decimal32 x;		/* { dg-error "" } */
_Decimal64 y;		/* { dg-error "" } */
_Decimal128 z;		/* { dg-error "" } */
