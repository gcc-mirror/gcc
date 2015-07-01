/* PR target/66560 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

typedef float v8sf __attribute__((vector_size(32)));
typedef int v8si __attribute__((vector_size(32)));
v8sf foo1 (v8sf x, v8sf y)
{
  v8sf tem0 = x - y;
  v8sf tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v8si) { 0, 9, 2, 11, 4, 13, 6, 15 });
}

v8sf foo2 (v8sf x, v8sf y)
{
  v8sf tem0 = x - y;
  v8sf tem1 = y + x;
  return __builtin_shuffle (tem0, tem1, (v8si) { 0, 9, 2, 11, 4, 13, 6, 15 });
}

v8sf foo3 (v8sf x, v8sf y)
{
  v8sf tem0 = x + y;
  v8sf tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v8si) { 8, 1, 10, 3, 12, 5, 14, 7 });
}

v8sf foo4 (v8sf x, v8sf y)
{
  v8sf tem0 = y + x;
  v8sf tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v8si) { 8, 1, 10, 3, 12, 5, 14, 7 });
}

/* { dg-final { scan-assembler-times "vaddsubps" 4 } } */
