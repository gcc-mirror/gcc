/* PR target/56766 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

typedef float v4sf __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
v4sf foo_v4sf (v4sf x, v4sf y)
{
  v4sf tem0 = x - y;
  v4sf tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v4si) { 0, 5, 2, 7 });
}

typedef float v8sf __attribute__((vector_size(32)));
typedef int v8si __attribute__((vector_size(32)));
v8sf foo_v8sf (v8sf x, v8sf y)
{
  v8sf tem0 = x - y;
  v8sf tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v8si) { 0, 9, 2, 11, 4, 13, 6, 15 });
}

typedef double v2df __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));
v2df foo_v2df (v2df x, v2df y)
{
  v2df tem0 = x - y;
  v2df tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v2di) { 0, 3 });
}

typedef double v4df __attribute__((vector_size(32)));
typedef long long v4di __attribute__((vector_size(32)));
v4df foo_v4df (v4df x, v4df y)
{
  v4df tem0 = x - y;
  v4df tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v4di) { 0, 5, 2, 7 });
}

/* { dg-final { scan-assembler-times "vaddsubps" 2 } } */
/* { dg-final { scan-assembler-times "vaddsubpd" 2 } } */
