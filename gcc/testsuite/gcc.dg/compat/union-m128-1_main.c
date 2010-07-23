/* { dg-skip-if "test SSE2 support" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O" } */
/* { dg-require-effective-target sse2_runtime } */

/* Test function argument passing.  PR target/15301.  */

extern void union_m128_1_x (void);
extern void exit (int);

int
main ()
{
  union_m128_1_x ();
  exit (0);
}
