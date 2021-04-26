/* PR target/98911  */
/* { dg-do compile } */
/* { dg-options "-O -mavx2" } */
/* { dg-final { scan-assembler-times "pcmpeqb" 2 } } */
/* { dg-final { scan-assembler-times "pcmpeqw" 2 } } */
/* { dg-final { scan-assembler-times "pcmpeqd" 2 } } */
/* { dg-final { scan-assembler-times "pcmpeqq" 2 } } */
/* { dg-final { scan-assembler-times "pcmpgtb" 2 } } */
/* { dg-final { scan-assembler-times "pcmpgtw" 2 } } */
/* { dg-final { scan-assembler-times "pcmpgtd" 2 } } */
/* { dg-final { scan-assembler-times "pcmpgtq" 2 } } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v32qi __attribute__ ((vector_size (32)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef long long v2di __attribute__ ((vector_size (16)));
typedef long long v4di __attribute__ ((vector_size (32)));

v16qi
f1 (v16qi a, v16qi b)
{
  return __builtin_ia32_pcmpeqb128 (a, b);
}

v8hi
f2 (v8hi a, v8hi b)
{
  return __builtin_ia32_pcmpeqw128 (a, b);
}

v4si
f3 (v4si a, v4si b)
{
  return __builtin_ia32_pcmpeqd128 (a, b);
}

v2di
f4 (v2di a, v2di b)
{
  return __builtin_ia32_pcmpeqq (a, b);
}

v16qi
f5 (v16qi a, v16qi b)
{
  return __builtin_ia32_pcmpgtb128 (a, b);
}

v8hi
f6 (v8hi a, v8hi b)
{
  return __builtin_ia32_pcmpgtw128 (a, b);
}

v4si
f7 (v4si a, v4si b)
{
  return __builtin_ia32_pcmpgtd128 (a, b);
}

v2di
f8 (v2di a, v2di b)
{
  return __builtin_ia32_pcmpgtq (a, b);
}

v32qi
f9 (v32qi a, v32qi b)
{
  return __builtin_ia32_pcmpeqb256 (a, b);
}

v16hi
f10 (v16hi a, v16hi b)
{
  return __builtin_ia32_pcmpeqw256 (a, b);
}

v8si
f11 (v8si a, v8si b)
{
  return __builtin_ia32_pcmpeqd256 (a, b);
}

v4di
f12 (v4di a, v4di b)
{
  return __builtin_ia32_pcmpeqq256 (a, b);
}

v32qi
f13 (v32qi a, v32qi b)
{
  return __builtin_ia32_pcmpgtb256 (a, b);
}

v16hi
f14 (v16hi a, v16hi b)
{
  return __builtin_ia32_pcmpgtw256 (a, b);
}

v8si
f15 (v8si a, v8si b)
{
  return __builtin_ia32_pcmpgtd256 (a, b);
}

v4di
f16 (v4di a, v4di b)
{
  return __builtin_ia32_pcmpgtq256 (a, b);
}
