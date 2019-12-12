/* PR target/88547 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3" } */
/* { dg-final { scan-assembler-not "pmingtw\[\t ]" } } */
/* { dg-final { scan-assembler-times "pminub\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "pminsw\[\t ]" 2 } } */
/* { dg-final { scan-assembler-not "pminsb\[\t ]" } } */
/* { dg-final { scan-assembler-not "pminuw\[\t ]" } } */
/* { dg-final { scan-assembler-not "pminud\[\t ]" } } */
/* { dg-final { scan-assembler-not "pminuq\[\t ]" } } */

typedef signed char v16qi __attribute__((vector_size(16)));
typedef unsigned char v16uqi __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
typedef unsigned short v8uhi __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
typedef unsigned v4usi __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));
typedef unsigned long long v2udi __attribute__((vector_size(16)));

v16qi
f1 (v16qi x, v16qi y)
{
  return x <= y;
}

v16uqi
f2 (v16uqi x, v16uqi y)
{
  return x <= y;
}

v16qi
f3 (v16qi x, v16qi y)
{
  return x >= y;
}

v16uqi
f4 (v16uqi x, v16uqi y)
{
  return x >= y;
}

v8hi
f5 (v8hi x, v8hi y)
{
  return x <= y;
}

v8uhi
f6 (v8uhi x, v8uhi y)
{
  return x <= y;
}

v8hi
f7 (v8hi x, v8hi y)
{
  return x >= y;
}

v8uhi
f8 (v8uhi x, v8uhi y)
{
  return x >= y;
}

v4si
f9 (v4si x, v4si y)
{
  return x <= y;
}

v4usi
f10 (v4usi x, v4usi y)
{
  return x <= y;
}

v4si
f11 (v4si x, v4si y)
{
  return x >= y;
}

v4usi
f12 (v4usi x, v4usi y)
{
  return x >= y;
}

v2di
f13 (v2di x, v2di y)
{
  return x <= y;
}

v2udi
f14 (v2udi x, v2udi y)
{
  return x <= y;
}

v2di
f15 (v2di x, v2di y)
{
  return x >= y;
}

v2udi
f16 (v2udi x, v2udi y)
{
  return x >= y;
}
