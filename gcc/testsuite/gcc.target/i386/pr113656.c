/* PR rtl-optimization/113656 */
/* { dg-do compile } */
/* { dg-options "-O3 -frounding-math -funsafe-math-optimizations -mavx512fp16 -mavx512vl" } */

_Float16 a[8];

void
foo ()
{
  for (int i = 0; i < 8; i++)
    a[i] = i - 8.4;
}
