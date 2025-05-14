/* Bug 117164: ICE with -std=gnu23 inlining function containing call
   to non-inlined nested function returning variable-size struct.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-guess-branch-probability -std=gnu23" } */

void
foo (int n)
{
  struct S { int a[n]; };

  __attribute__ ((noinline)) struct S
  fn (void)
  {
    struct S s;
    s.a[0] = 42;
    return s;
  }

  fn ();
}

int
main (void)
{
  foo (1);
}
