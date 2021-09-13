/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-Og -mvsx" } */

vector int
foo (vector int v)
{
  for (long k = 0; k < 1; ++k)
    v[k] = 0;
  return v;
}
