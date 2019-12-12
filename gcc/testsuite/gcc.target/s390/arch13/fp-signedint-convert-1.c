/* { dg-compile } */

typedef int __attribute__((vector_size(16))) v4si;
typedef float __attribute__((vector_size(16))) v4sf;

v4si
toint (v4sf a)
{
  v4si out = (v4si){ (int)a[0], (int)a[1], (int)a[2], (int)a[3] };
  return out;
}

/* { dg-final { scan-assembler-times "vcfeb\t%v24,%v24,0,5" 1 } } */

v4sf
tofloat (v4si a)
{
  v4sf out = (v4sf){ (float)a[0], (float)a[1], (float)a[2], (float)a[3] };
  return out;
}

/* { dg-final { scan-assembler-times "vcefb\t%v24,%v24,0,0" 1 } } */
