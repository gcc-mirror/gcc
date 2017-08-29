/* PR target/39431 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-march=i686 -fpic" { target { ia32 && fpic } } } */

extern void bar (char *, int);

int
foo (long long *p, long long oldv, long long *q, int n)
{
  char buf[n];
  bar (buf, n);
  p[256 + n] = __sync_val_compare_and_swap (p + n, oldv, oldv + 6);
  return __sync_bool_compare_and_swap (q + n, oldv, oldv + 8);
}
