/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef short S;
typedef S V __attribute__((vector_size(2 * sizeof(S))));

V duplicate (S a)
{
  return (V) { a, a };
}

V one_nonzero (S a)
{
  return (V) { 0, a };
}

V one_var (S a)
{
  return (V) { 1, a };
}

V general (S a, S b)
{
  return (V) { a, b };
}
