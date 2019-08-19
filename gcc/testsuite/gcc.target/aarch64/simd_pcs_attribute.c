/* { dg-do compile } */
/* { dg-options "-Ofast" } */

__attribute__ ((__simd__ ("notinbranch")))
__attribute__ ((__nothrow__ , __leaf__ , __const__))
extern double log (double __x);

void foo(double *f, int n)
{
	int i;
	for (i = 0; i < n; i++)
		f[i] = log(f[i]);
}

/* { dg-final { scan-assembler-not {\.variant_pcs\tlog} } } */
/* { dg-final { scan-assembler-times {\.variant_pcs\t_ZGVnN2v_log} 1 } } */
