/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

_Float16 a
  = 1.0F16;
__extension__ _Float16 b
  = 2.0F16;
