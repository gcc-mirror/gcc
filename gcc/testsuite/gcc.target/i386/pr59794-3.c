/* PR target/59794 */
/* { dg-prune-output "ABI for passing parameters" } */
/* { dg-options "-O2 -mno-avx" } */
/* { dg-skip-if "no AVX vector" { *-*-mingw* } } */

typedef int __v8si __attribute__ ((__vector_size__ (32)));

extern __v8si x;

extern void bar (__v8si);
void
foo (void)
{
  bar (x); /* { dg-message "warning: AVX vector argument without AVX enabled changes the ABI" } */
}
