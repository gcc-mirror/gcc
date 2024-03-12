/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

_Float16 a		/* { dg-error "ISO C does not support the '_Float16' type before C2X" } */
  = 1.0F16;		/* { dg-error "non-standard suffix on floating constant before C2X" } */
__extension__ _Float16 b
  = 2.0F16;
