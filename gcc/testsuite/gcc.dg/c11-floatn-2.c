/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

_Float128 a		/* { dg-error "ISO C does not support the '_Float128' type before C2X" } */
  = 1.0F128;		/* { dg-error "non-standard suffix on floating constant before C2X" } */
__extension__ _Float128 b
  = 2.0F128;
