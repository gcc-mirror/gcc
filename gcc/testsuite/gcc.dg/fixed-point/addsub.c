/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* Check if GCC generates warnings when overflows.  */

_Fract a0 = 0.5r + 0.5r; /* { dg-warning "overflow" } */
_Fract a1 = 0.5r + 0.6r; /* { dg-warning "overflow" } */
_Fract a2 = 0.5r + -0.6r;
_Fract a3 = -0.5r + 0.6r;
_Fract a4 = -0.5r + -0.5r;
_Fract a5 = -0.5r + -0.6r; /* { dg-warning "overflow" } */
_Fract a6 = 0.0r + __FRACT_MIN__;
_Fract a7 = 0.1r + __FRACT_MIN__;
_Fract a8 = -0.1r + __FRACT_MIN__; /* { dg-warning "overflow" } */
_Fract a9 = 0.0r + __FRACT_MAX__;
_Fract a10 = 0.1r + __FRACT_MAX__; /* { dg-warning "overflow" } */
_Fract a11 = -0.1r + __FRACT_MAX__;

_Fract b0 = 0.5r - 0.5r;
_Fract b1 = 0.5r - 0.6r;
_Fract b2 = 0.5r - -0.6r; /* { dg-warning "overflow" } */
_Fract b3 = -0.5r - 0.6r; /* { dg-warning "overflow" } */
_Fract b4 = -0.5r - -0.5r;
_Fract b5 = -0.5r - -0.6r;
_Fract b6 = 0.0r - __FRACT_MIN__; /* { dg-warning "overflow" } */
_Fract b7 = 0.1r - __FRACT_MIN__; /* { dg-warning "overflow" } */
_Fract b8 = -0.1r - __FRACT_MIN__;
_Fract b9 = 0.0r - __FRACT_MAX__;
_Fract b10 = 0.1r - __FRACT_MAX__;
_Fract b11 = -0.1r - __FRACT_MAX__; /* { dg-warning "overflow" } */
