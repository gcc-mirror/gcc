/* { dg-do compile { target { ! { *-*-darwin* hppa*-*-* } } } } */
/* { dg-options "-fpie" } */

#if __PIC__ != 1
# error __PIC__ is not 1!
#endif

#if __PIE__ != 1
# error __PIE__ is not 1!
#endif
