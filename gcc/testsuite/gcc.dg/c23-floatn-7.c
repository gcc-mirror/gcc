/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

_Float16 a		/* { dg-warning "ISO C does not support the '_Float16' type before C23" } */
  = 1.0F16;		/* { dg-warning "non-standard suffix on floating constant before C23" } */
__extension__ _Float16 b
  = 2.0F16;
