/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

__attribute__ ((__gnu_inline__, __always_inline__, target("avx512ifma,avx512vl")))
inline int
foo (void) /* { dg-error "inlining failed in call to 'always_inline' .* target specific option mismatch" } */
{
  return 0;
}

__attribute__ ((target("avxifma")))
int
bar (void)
{
  return foo (); /* { dg-message "called from here" } */
}
