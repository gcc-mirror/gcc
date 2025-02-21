/* PR target/88620 */
/* { dg-do run } */
/* { dg-options "-Ofast --param ipa-cp-eval-threshold=0 -fno-guess-branch-probability -fno-inline-small-functions" } */

void
foo (int n)
{
  struct S { int a[n]; };

  struct S
  fn (void)
  {
    struct S s;
    s.a[0] = 42;
    return s;
  }

  auto struct S
  fn2 (void)
  {
    return fn ();
  }

  struct S x;
  fn ();
  fn2 ();
  x = fn ();

  if (x.a[0] != 42)
    __builtin_abort ();

  if (fn ().a[0] != 42)
    __builtin_abort ();

  __typeof__ (fn ()) *p = &x;
  if (p->a[0] != 42)
    __builtin_abort ();

  if (fn2 ().a[0] != 42)
    __builtin_abort ();
}

int
main (void)
{
  foo (1);
}
