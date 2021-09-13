/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-mno-80387" } */

inline double
foo (void)
{
  return 1.0;
}
