/* { dg-do compile } */
/* { dg-options "-mcpu=ev56 -mno-bwx" } */

#ifdef __alpha_bwx__
# error "BWX enabled"
#endif
