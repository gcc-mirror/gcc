/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int x;

float __GIMPLE ()
foo (int * p, __UINTPTR_TYPE__ idx, __UINTPTR_TYPE__ idx2)
{
  float f;
  float D1800;
  unsigned int D1799;

  D1799 = __MEM <unsigned int, 8> ((char *)p + 1 + idx * _Literal (__SIZETYPE__) 2);
  __MEM <unsigned int, 16> ((char *)&f + 0xfffffffffffffffe) = D1799;
  __MEM <int> (&x + idx2) = 1;
  __MEM <int, 2> (p + idx * _Literal (__SIZETYPE__) 1) = 1;
  __MEM <int> (&x + 2 + idx2) = 1;
  __MEM <int> ((char *)&x + 4 + idx * _Literal (__SIZETYPE__) 4 + idx2) = 1;
  D1800 = f;
  return D1800;
}
