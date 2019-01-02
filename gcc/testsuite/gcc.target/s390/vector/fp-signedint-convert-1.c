/* { dg-compile } */
/* { dg-options "-O3 -march=z13 -mzarch" } */

typedef long long __attribute__((vector_size(16))) v2di;
typedef double __attribute__((vector_size(16))) v2df;

v2di longvec;
v2df doublevec;

v2di
tolong (v2df a)
{
  v2di out = (v2di){ (long long)a[0], (long long)a[1] };
  return out;
}

/* { dg-final { scan-assembler-times "vcgdb\t%v24,%v24,0,5" 1 } } */

v2df
todouble (v2di a)
{
  v2df out = (v2df){ (double)a[0], (double)a[1] };
  return out;
}

/* { dg-final { scan-assembler-times "vcdgb\t%v24,%v24,0,0" 1 } } */
