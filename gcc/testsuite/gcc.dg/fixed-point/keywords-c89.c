/* { dg-do compile } */
/* { dg-options "-std=c89" } */

/* Flixed-point keywords are not recognized in C89 mode.  */

_Fract w;		/* { dg-error "" } */
_Accum x;		/* { dg-error "" } */
_Sat _Fract y;		/* { dg-error "" } */
_Sat _Accum z;		/* { dg-error "" } */
