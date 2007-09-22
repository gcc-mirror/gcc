/* { dg-do compile } */
/* { dg-options "-std=c99" } */

/* Fixed-point keywords are not recognized in C99 mode.  */

_Fract w;		/* { dg-error "" } */
_Accum x;		/* { dg-error "" } */
_Sat _Fract y;		/* { dg-error "" } */
_Sat _Fract z;		/* { dg-error "" } */
