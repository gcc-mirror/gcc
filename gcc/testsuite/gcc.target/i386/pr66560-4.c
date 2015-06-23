/* PR target/66560 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

typedef double v4df __attribute__((vector_size(32)));
typedef long long v4di __attribute__((vector_size(32)));
v4df foo1 (v4df x, v4df y)
{
  v4df tem0 = x - y;
  v4df tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v4di) { 0, 5, 2, 7 });
}

v4df foo2 (v4df x, v4df y)
{
  v4df tem0 = x - y;
  v4df tem1 = y + x;
  return __builtin_shuffle (tem0, tem1, (v4di) { 0, 5, 2, 7 });
}

v4df foo3 (v4df x, v4df y)
{
  v4df tem0 = x + y;
  v4df tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v4di) { 4, 1, 6, 3 });
}

v4df foo4 (v4df x, v4df y)
{
  v4df tem0 = y + x;
  v4df tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v4di) { 4, 1, 6, 3 });
}

/* { dg-final { scan-assembler-times "vaddsubpd" 4 } } */
