/* { dg-do compile } */
/* { dg-options "-march=sierraforest -O2" } */
/* { dg-final { scan-assembler-not {(?n)vp?blendv(b|ps|pd)} } } */

void
foo (int* a, int* b, int* __restrict c)
{
  for (int i = 0; i != 200; i++)
    {
      c[i] += a[i] > b[i] ? 1 : -1;
    }
}
