/* { dg-do compile } */
/* { dg-options "-fPIE" } */

#if __PIC__ != 2
# error __PIC__ is not 2!
#endif

#if __PIE__ != 2
# error __PIE__ is not 2!
#endif
