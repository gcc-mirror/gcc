/* { dg-do compile } */
/* { dg-options "-mcpu=ev56" } */

#ifdef __alpha_max__
# error "MAX enabled"
#endif
