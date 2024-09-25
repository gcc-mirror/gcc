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
  v16qi  = vec_splat_s8 (-112);
  uv16qi = vec_splat_u8 (215);

  v8hi  = vec_splat_s16 (-32000);
  uv8hi = vec_splat_u16 (64000);

  v4si  = vec_splat_s32 (-32000);
  uv4si = vec_splat_u32 (64000);

  v2di  = vec_splat_s64 (-32000);
  uv2di = vec_splat_u64 (64000);
}

/* { dg-final { scan-assembler-times "vrepi\t%v.*,-112,0" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,-41,0" 1 } } */

/* { dg-final { scan-assembler-times "vrepi\t%v.*,-32000,1" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,-1536,1" 1 } } */

/* { dg-final { scan-assembler-times "vrepi\t%v.*,-32000,2" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,-1536,2" 1 } } */

/* { dg-final { scan-assembler-times "vrepi\t%v.*,-32000,3" 1 } } */
/* { dg-final { scan-assembler-times "vrepi\t%v.*,-1536,3" 1 } } */
