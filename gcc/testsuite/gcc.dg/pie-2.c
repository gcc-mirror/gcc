/* { dg-do compile } */
/* { dg-options "-fPIE" } */
/* { dg-require-effective-target pie } */
/* { dg-skip-if "__PIC__ is always 1 for MIPS" { mips*-*-* } } */
/* { dg-skip-if "__PIE__ is often not defined on darwin" { *-*-darwin* } } */

#if __PIC__ != 2
# error __PIC__ is not 2!
#endif

#if __PIE__ != 2
# error __PIE__ is not 2!
#endif
