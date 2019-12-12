/* { dg-do compile } */
/* { dg-options "-Ofast" } */

__attribute__ ((__simd__ ("notinbranch")))
__attribute__ ((__nothrow__ , __leaf__ , __const__))
extern double foo (double x);

void bar(double * f, int n)
{
	int i;
	for (i = 0; i < n; i++)
		f[i] = foo(f[i]);
}

/* { dg-final { scan-assembler-not {\.variant_pcs\tfoo} } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\t_ZGVnN2v_foo} 1 } } */
