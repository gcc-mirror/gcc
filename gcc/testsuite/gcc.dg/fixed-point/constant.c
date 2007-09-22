/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1169 6.4.4.2a - Fixed-point constants (NEW CLAUSE).

   Check if fixed-point constants are ok.  */

_Accum k0 = 0k;
_Accum k1 = 9999.0e0k;
_Accum k2 = 9999.0k;
_Accum k3 = 9999k;
_Accum k4 = 9999e0k;
_Accum k5 = 09999k;
_Accum k6 = 09999e0k;
_Accum k7 = 09999.0e0k;
_Accum k8 = 09999.0k;
_Accum k9 = 0x270fp0k;
_Accum k10 = 0x270f.0p0k;

_Fract r0 = 0r;
_Fract r1 = 0e0r;
_Fract r2 = 0.0e0r;
_Fract r3 = 0.1e0r;
_Fract r4 = 0.0r;
_Fract r5 = 0.1r;
_Fract r6 = 0x0p0r;
_Fract r7 = 0x0.0p0r;
_Fract r8 = 0x0.1p0r;
