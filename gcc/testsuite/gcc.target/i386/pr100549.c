/* PR target/100549  */
/* { dg-do compile } */
/* { dg-options "-O -mavx2" } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v32qi __attribute__ ((vector_size (32)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef long long v2di __attribute__ ((vector_size (16)));
typedef long long v4di __attribute__ ((vector_size (32)));

v16qi
f1 (v16qi a)
{
  return __builtin_ia32_pcmpeqb128 (a, a);
}

v8hi
f2 (v8hi a)
{
  return __builtin_ia32_pcmpeqw128 (a, a);
}

v4si
f3 (v4si a)
{
  return __builtin_ia32_pcmpeqd128 (a, a);
}

v2di
f4 (v2di a)
{
  return __builtin_ia32_pcmpeqq (a, a);
}

v16qi
f5 (v16qi a)
{
  return __builtin_ia32_pcmpgtb128 (a, a);
}

v8hi
f6 (v8hi a)
{
  return __builtin_ia32_pcmpgtw128 (a, a);
}

v4si
f7 (v4si a)
{
  return __builtin_ia32_pcmpgtd128 (a, a);
}

v2di
f8 (v2di a)
{
  return __builtin_ia32_pcmpgtq (a, a);
}

v32qi
f9 (v32qi a)
{
  return __builtin_ia32_pcmpeqb256 (a, a);
}

v16hi
f10 (v16hi a)
{
  return __builtin_ia32_pcmpeqw256 (a, a);
}

v8si
f11 (v8si a)
{
  return __builtin_ia32_pcmpeqd256 (a, a);
}

v4di
f12 (v4di a)
{
  return __builtin_ia32_pcmpeqq256 (a, a);
}

v32qi
f13 (v32qi a)
{
  return __builtin_ia32_pcmpgtb256 (a, a);
}

v16hi
f14 (v16hi a)
{
  return __builtin_ia32_pcmpgtw256 (a, a);
}

v8si
f15 (v8si a)
{
  return __builtin_ia32_pcmpgtd256 (a, a);
}

v4di
f16 (v4di a)
{
  return __builtin_ia32_pcmpgtq256 (a, a);
}
