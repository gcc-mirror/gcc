/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-msoft-float -mfp-ret-in-387" } */

void f() {
  __builtin_apply(0, 0, 0);
}
