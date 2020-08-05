/* { dg-do compile } */
/* { dg-options "-march=skylake-avx512 -O2" } */
/* { dg-final { scan-assembler-times "cmp" 2 } } */
/* { dg-final { scan-assembler-not "and" } } */

typedef long vec __attribute__((vector_size(16)));
vec f(vec x, vec y){
  return (x < 5) & (y < 8);
}

/* On x86_64, currently
	vpcmpq	$2, .LC1(%rip), %xmm1, %k1
	vpcmpq	$2, .LC0(%rip), %xmm0, %k0{%k1}
	vpmovm2q	%k0, %xmm0
*/
