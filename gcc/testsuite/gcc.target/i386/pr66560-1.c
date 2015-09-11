/* PR target/66560 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4" } */

typedef float v4sf __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
v4sf foo1 (v4sf x, v4sf y)
{
  v4sf tem0 = x - y;
  v4sf tem1 = x + y;
  return __builtin_shuffle (tem0, tem1, (v4si) { 0, 5, 2, 7 });
}

v4sf foo2 (v4sf x, v4sf y)
{
  v4sf tem0 = x - y;
  v4sf tem1 = y + x;
  return __builtin_shuffle (tem0, tem1, (v4si) { 0, 5, 2, 7 });
}

v4sf foo3 (v4sf x, v4sf y)
{
  v4sf tem0 = x + y;
  v4sf tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v4si) { 4, 1, 6, 3 });
}

v4sf foo4 (v4sf x, v4sf y)
{
  v4sf tem0 = y + x;
  v4sf tem1 = x - y;
  return __builtin_shuffle (tem0, tem1, (v4si) { 4, 1, 6, 3 });
}

/* { dg-final { scan-assembler-times "addsubps" 4 } } */
