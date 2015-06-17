/* PR target/60205 */
/* { dg-do compile } */
/* { dg-prune-output "ABI for passing parameters" } */
/* { dg-options "-O2 -mno-avx512f" } */
/* { dg-skip-if "no AVX512F vector" { *-*-mingw* } } */

typedef int __v16si __attribute__ ((__vector_size__ (64)));

extern __v16si x;

extern void bar (__v16si);
void
foo (void)
{
  bar (x); /* { dg-message "warning: AVX512F vector argument without AVX512F enabled changes the ABI" } */
}
