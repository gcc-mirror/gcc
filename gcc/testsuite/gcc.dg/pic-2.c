/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fPIC" } */
/* { dg-skip-if "__PIC__ is always 1 for MIPS" { mips*-*-* } } */
/* { dg-skip-if "__PIE__ is always defined for GCN" { amdgcn*-*-* } } */

#if defined(__CYGWIN__) || defined(__WIN32__)
# if __PIC__ != 1
#  error __PIC__ is not 1!
# endif
#elif __PIC__ != 2
# error __PIC__ is not 2!
#endif

#ifdef __PIE__
# error __PIE__ is defined!
#endif
