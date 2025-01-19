/* { dg-do compile } */
/* { dg-options "-mcpu=ev6 -mno-fix" } */

#ifdef __alpha_fix__
# error "FIX enabled"
#endif
