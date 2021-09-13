/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-mgeneral-regs-only" } */

inline double
foo (void)
{
  return 1.0;
}
