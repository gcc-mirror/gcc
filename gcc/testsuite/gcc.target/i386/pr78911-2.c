/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */
/* { dg-additional-options "-fPIC" { target fpic } } */
/* { dg-additional-options "-march=i686" { target ia32 } } */

long long *a, *b, c;
int d, e;
int baz (void);

static inline long long
foo (long long *x)
{
  return __sync_val_compare_and_swap (x, 0, 0);
}

void
bar ()
{
  int f = baz ();
  c = foo (&a[f]);
  if (c)
    e = d;
  a = b;
}
