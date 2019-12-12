/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("avx512vl", "avx512bw", "avx512dq",
			     "avx512cd", "avx512er", "avx512pf", "avx512vbmi",
			     "avx512ifma", "avx5124vnniw", "avx5124fmaps",
			     "avx512vpopcntdq", "avx512vbmi2", "gfni",
			     "vpclmulqdq", "avx512vnni", "avx512bitalg",
			     "default")))
int foo (); /* { dg-error "ISA '\[^\n\r\]*' is not supported in 'target' attribute, use 'arch=' syntax" } */

int
bar ()
{
  return foo();
}
