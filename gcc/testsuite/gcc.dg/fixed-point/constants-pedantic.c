/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

/* N1169 6.4.4.2a - Fixed-point constants (NEW CLAUSE).  */

_Accum k0 = 0k;		/* { dg-warning "GCC extension|ISO C" } */
_Accum k1 = 9999.0e0k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k2 = 9999.0k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k3 = 9999k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k4 = 9999e0k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k5 = 09999k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k6 = 09999e0k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k7 = 09999.0e0k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k8 = 09999.0k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k9 = 0x270fp0k;	/* { dg-warning "GCC extension|ISO C" } */
_Accum k10 = 0x270f.0p0k;	/* { dg-warning "GCC extension|ISO C" } */

_Fract r0 = 0r;		/* { dg-warning "GCC extension|ISO C" } */
_Fract r1 = 0e0r;	/* { dg-warning "GCC extension|ISO C" } */
_Fract r2 = 0.0e0r;	/* { dg-warning "GCC extension|ISO C" } */
_Fract r3 = 0.1e0r;	/* { dg-warning "GCC extension|ISO C" } */
_Fract r4 = 0.0r;	/* { dg-warning "GCC extension|ISO C" } */
_Fract r5 = 0.1r;	/* { dg-warning "GCC extension|ISO C" } */
_Fract r6 = 0x0p0r;	/* { dg-warning "GCC extension|ISO C" } */
_Fract r7 = 0x0.0p0r;	/* { dg-warning "GCC extension|ISO C" } */
_Fract r8 = 0x0.1p0r;	/* { dg-warning "GCC extension|ISO C" } */
