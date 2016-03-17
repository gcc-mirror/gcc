/* { dg-do compile } */
/* { dg-options "-fPIC" } */

#if __PIC__ != 2
# error __PIC__ is not 2!
#endif

#ifdef __PIE__
# error __PIE__ is defined!
#endif
