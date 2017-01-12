/* { dg-do compile } */
/* { dg-options "-fgimple" } */

float __GIMPLE ()
foo (int * p)
{
  float f;
  float D1800;
  unsigned int D1799;

  D1799 = __MEM <unsigned int, 8> ((char *)p + 1);
  __MEM <unsigned int, 16> ((char *)&f + 0xfffffffffffffffe) = D1799;
  __MEM <int> (p) = 1;
  __MEM <int, 2> (p) = 1;
  __MEM <int> (p + 2) = 1;
  __MEM <int> ((char *)p) = 1;
  D1800 = f;
  return D1800;
}
