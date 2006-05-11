/* This is a compile-only test for interaction of "-maltivec" and "-save-temps".  */
/* Author:  Ziemowit Laski  <zlaski@apple.com>.  */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-save-temps -maltivec" } */

#include <altivec.h>

#define vector_float vector float
#define vector_float_foo vector float foo
#define vector_float_bar_eq vector float bar =

/* NB: Keep the following split across three lines.  */
vector
int
a1 = { 100, 200, 300, 400 };

vector_float f1 = { 1.0, 2.0, 3.0, 4.0 };  
vector_float_foo = { 3.0, 4.0, 5.0, 6.0 };
vector_float_bar_eq { 8.0, 7.0, 6.0, 5.0 };

/* { dg-final { cleanup-saved-temps } } */
