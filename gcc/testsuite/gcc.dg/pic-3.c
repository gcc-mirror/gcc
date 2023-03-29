/* { dg-do compile { target { ! { *-*-darwin* hppa*64*-*-* mips*-*-linux-* amdgcn*-*-* } } } } */
/* { dg-options "-fno-pic" } */

#if defined(__CYGWIN__) || defined(__WIN32__)
# if __PIC__ != 1
#  error __PIC__ is not 1!
# endif
#elif __PIC__
# error __PIC__ is defined!
#endif

#ifdef __PIE__
# error __PIE__ is defined!
#endif
