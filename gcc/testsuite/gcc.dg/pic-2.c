/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC" } */
/* { dg-skip-if "__PIC__ is always 1 for MIPS" { mips*-*-* } } */

#if __PIC__ != 2
# error __PIC__ is not 2!
#endif

#ifdef __PIE__
# error __PIE__ is defined!
#endif
