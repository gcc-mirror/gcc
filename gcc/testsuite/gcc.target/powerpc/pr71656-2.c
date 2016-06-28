/* Test for reload ICE arising from POWER9 Vector Dform code generation.  */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-O3 -mcpu=power9 -mpower9-dform-vector -mno-lra -funroll-loops -fno-aggressive-loop-optimizations" } */

typedef double vec[3];
struct vec_t
{
  vec x;
  vec y;
};
int a, j, k, l, m, n, o, p, q;
double b, i;
vec c;
double h[6];
void func1 (vec);

void
func2 (double *)
{
  for (; k; k--)
    for (; j <= k;)
      for (; m <= q; m++)
	for (; n <= k; n++)
	  for (; o <= l; o++)
	    {
	      j = p + m + n + o;
	      h[j] = i;
	    }
}

void
func3 (void)
{
  vec_t d;
  func1 (d.y);
  func2 (&b);
  for (; a;)
    {
      double *e = d.y, *g;
      double f;
      c[0] = g[0] + f * e[0];
      c[1] = g[1] + f * e[1];
      func1 (c);
    }
}
