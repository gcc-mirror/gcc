/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-O2 -mvsx -mno-float128 -save-temps" } */
/* { dg-require-effective-target powerpc_vsx } */
#include "float128-3.c"
