/* { dg-do compile { target {! fixed_point} } } */
/* { dg-options "-std=gnu99" } */

_Fract w;       /* { dg-error "not supported" "reject fixed-point" } */
_Sat _Fract x;  /* { dg-error "not supported" "reject fixed-point" } */
_Accum y;       /* { dg-error "not supported" "reject fixed-point" } */
_Sat _Accum z;  /* { dg-error "not supported" "reject fixed-point" } */
