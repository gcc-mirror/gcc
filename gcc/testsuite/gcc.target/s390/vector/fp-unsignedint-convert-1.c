/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzarch" } */

typedef unsigned long long __attribute__((vector_size(16))) v2di;
typedef double __attribute__((vector_size(16))) v2df;

v2di longvec;
v2df doublevec;

v2di
toulong (v2df a)
{
  v2di out = (v2di){ (unsigned long long)a[0], (unsigned long long)a[1] };
  return out;
}

/* { dg-final { scan-assembler-times "vclgdb\t%v24,%v24,0,5" 1 } } */

v2df
todouble (v2di a)
{
  v2df out = (v2df){ (double)a[0], (double)a[1] };
  return out;
}

/* { dg-final { scan-assembler-times "vcdlgb\t%v24,%v24,0,0" 1 } } */
