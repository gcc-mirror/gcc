/* { dg-do compile } */
/* { dg-options "-mcpu=ev67 -mno-cix" } */

#ifdef __alpha_cix__
# error "CIX enabled"
#endif
