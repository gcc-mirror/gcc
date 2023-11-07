/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

_Float128 a
  = 1.0F128;
__extension__ _Float128 b
  = 2.0F128;
