/* { dg-do compile } */
/* { dg-options "-O2 -march=native -mno-apxf" } */

#ifdef __APX_F__
# error APX_F should be disabled
#endif
