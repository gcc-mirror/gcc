/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-strong -mdejagnu-cpu=power8" } */
/* { dg-require-effective-target ppc_ieee128_ok } */

typedef union
{
  __ieee128 a;
  int b;
} c;

__ieee128
d (__ieee128 x)
{
  __ieee128 g;
  c h;
  h.a = x;
  g = h.b & 5;
  h.b = g;
  if (g)
    return x - x;
  return h.a;
}
