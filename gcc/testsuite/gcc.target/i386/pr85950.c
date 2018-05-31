/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mfpmath=sse" } */

double floor (double);
double ceil (double);

int ifloor (double x) { return floor (x); }
int iceil (double x) { return ceil (x); }

#ifdef __x86_64__
long long llfloor (double x) { return floor (x); }
long long llceil (double x) { return ceil (x); }
#endif
  
/* { dg-final { scan-assembler-times "roundsd" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "roundsd" 4 { target { ! ia32 } } } } */
