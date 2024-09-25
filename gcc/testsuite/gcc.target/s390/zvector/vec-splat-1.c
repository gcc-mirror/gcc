/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector" } */

#include <vecintrin.h>

vector signed char v16qi;
vector short       v8hi;
vector int         v4si;
vector long long   v2di;

vector unsigned char      uv16qi;
vector unsigned short     uv8hi;
vector unsigned int       uv4si;
vector unsigned long long uv2di;

int
foo ()
{
  v16qi  = vec_splats ((signed char)0x77);
  uv16qi = vec_splats ((unsigned char)0x77);

  v8hi  = vec_splats ((short int)0x7f0f);
  uv8hi = vec_splats ((unsigned short int)0x7f0f);

  v4si  = vec_splats ((int)0x7f0f);
  uv4si = vec_splats ((unsigned int)0x7f0f);

  v2di  = vec_splats ((long long)0x7f0f);
  uv2di = vec_splats ((unsigned long long)0x7f0f);
}

/* { dg-final { scan-assembler-times "vrepi\t%v.*,119,0" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,119,0" 1 } } */

/* { dg-final { scan-assembler-times "vrepi\t%v.*,32527,1" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,32527,1" 1 } } */

/* { dg-final { scan-assembler-times "vrepi\t%v.*,32527,2" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,32527,2" 1 } } */

/* { dg-final { scan-assembler-times "vrepi\t%v.*,32527,3" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,32527,3" 1 } } */
