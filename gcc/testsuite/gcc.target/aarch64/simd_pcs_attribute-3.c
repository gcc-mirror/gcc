/* { dg-do compile } */
/* { dg-options "-Ofast" } */

__attribute__ ((__simd__))
__attribute__ ((__nothrow__ , __leaf__ , __const__))
double foo (double x);

void bar(double *f, int n)
{
	int i;
	for (i = 0; i < n; i++)
		f[i] = foo(f[i]);
}

double foo(double x)
{
	return x * x / 3.0;
}

/* { dg-final { scan-assembler-not {\.variant_pcs\tfoo} } } */
/* { dg-final { scan-assembler-not {\.variant_pcs\t_ZGVnM1v_foo} } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\t_ZGVnM2v_foo} 1 } } */
/* { dg-final { scan-assembler-not {\.variant_pcs\t_ZGVnN1v_foo} } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\t_ZGVnN2v_foo} 1 } } */
