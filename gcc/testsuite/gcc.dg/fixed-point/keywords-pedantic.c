/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

/* Fixed-point is a GCC extension.  */

_Fract w;		/* { dg-warning "GCC extension|ISO C" } */
_Accum x;		/* { dg-warning "GCC extension|ISO C" } */
_Sat _Fract y;		/* { dg-warning "GCC extension|ISO C" } */
_Sat _Accum z;		/* { dg-warning "GCC extension|ISO C" } */
