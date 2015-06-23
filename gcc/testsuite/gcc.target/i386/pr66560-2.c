/* PR target/66560 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4" } */

typedef double v2df __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));
v2df foo1 (v2df x, v2df y)
{
  v2df tem0 = x - y;
  v2df tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v2di) { 0, 3 });
}

v2df foo2 (v2df x, v2df y)
{
  v2df tem0 = x - y;
  v2df tem1 = y + x;
  return __builtin_shuffle (tem0, tem1, (v2di) { 0, 3 });
}

v2df foo3 (v2df x, v2df y)
{
  v2df tem0 = x + y;
  v2df tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v2di) { 2, 1 });
}

v2df foo4 (v2df x, v2df y)
{
  v2df tem0 = y + x;
  v2df tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v2di) { 2, 1 });
}

/* { dg-final { scan-assembler-times "addsubpd" 4 } } */
