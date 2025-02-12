/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int v4si __attribute__((vector_size (sizeof(int) * 4)));

v4si x;

void foo (int flag)
{
  v4si tem = (v4si) { 0, 0, 0, 0 };
  if (flag)
    tem = (v4si) { flag };
  x = __builtin_shufflevector (tem, tem, 0, 0, 0, 0);
}
