/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch12" } */

typedef float       v4sf __attribute__((vector_size(16)));
typedef double      v2df __attribute__((vector_size(16)));
typedef long double v1tf __attribute__((vector_size(16)));

v4sf
neg_vfnmasb (v4sf a, v4sf b, v4sf c)
{
  return -(a * b + c);
}
/* { dg-final { scan-assembler-times "vfnmasb\t%v24,%v24,%v26,%v28" 1 } } */

v2df
neg_vfnmadb (v2df a, v2df b, v2df c)
{
  return -(a * b + c);
}
/* { dg-final { scan-assembler-times "vfnmadb\t%v24,%v24,%v26,%v28" 1 } } */

v1tf
neg_wfnmaxb (v1tf a, v1tf b, v1tf c)
{
  return -(a * b + c);
}
/* { dg-final { scan-assembler-times "wfnmaxb\t%v24,%v24,%v26,%v28" 1 } } */


v4sf
neg_vfnmssb (v4sf a, v4sf b, v4sf c)
{
  return -(a * b - c);
}
/* { dg-final { scan-assembler-times "vfnmssb\t%v24,%v24,%v26,%v28" 1 } } */

v2df
neg_vfnmsdb (v2df a, v2df b, v2df c)
{
  return -(a * b - c);
}
/* { dg-final { scan-assembler-times "vfnmsdb\t%v24,%v24,%v26,%v28" 1 } } */

v1tf
neg_wfnmsxb (v1tf a, v1tf b, v1tf c)
{
  return -(a * b - c);
}
/* { dg-final { scan-assembler-times "wfnmsxb\t%v24,%v24,%v26,%v28" 1 } } */
