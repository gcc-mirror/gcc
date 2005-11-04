/* PR rtl-optimization/12092  */
/* Test case reduced by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mtune=i486 -march=pentium4 -fprefetch-loop-arrays" } */

void DecodeAC(int index,int *matrix)
{
  int *mptr;

  for(mptr=matrix+index;mptr<matrix+64;mptr++) {*mptr = 0;}
}

