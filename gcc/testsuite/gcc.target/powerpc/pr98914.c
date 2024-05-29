/* { dg-do compile } */
/* { dg-options "-Og -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

vector int
foo (vector int v)
{
  for (long k = 0; k < 1; ++k)
    v[k] = 0;
  return v;
}
