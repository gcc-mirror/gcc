/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

static void foo(int dim,float _Complex f0[])
{
  int d;
  f0[0] -= 3.14;  /* { dg-bogus "uninitialized" } */
  for (d = 0; d < dim; ++d) f0[0] += 3.14;
}
void bar(int dim, const float _Complex u_t[], float _Complex f0[])
{
  float _Complex exp[1] = {0.};
  foo(dim, exp);
  f0[0] = u_t[0] - exp[0];
}
