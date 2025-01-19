/* { dg-do compile } */
/* { dg-options "-mcpu=pca56 -mno-max" } */

#ifdef __alpha_max__
# error "MAX enabled"
#endif
