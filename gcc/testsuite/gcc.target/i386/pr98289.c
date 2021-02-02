/* PR rtl-optimization/98289 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-blocks-and-partition -fdump-rtl-pro_and_epilogue-details" } */
/* { dg-final { scan-rtl-dump-times "Performing shrink-wrapping" 4 "pro_and_epilogue" } } */

int bar (void) __attribute__((cold));

void
foo (int x)
{
  if (x)
    __builtin_abort ();
}

void
baz (int x)
{
  if (__builtin_expect (x, 0))
    {
      bar ();
      bar ();
      bar ();
    }
}

void
qux (int x, int y, int z, int w)
{
  if (x || y || z || w)
    __builtin_abort ();
}

int
corge (int x, int y, int z, int w, int u)
{
  if (__builtin_expect (x, 0))
    goto lab;
  u++;
  if (__builtin_expect (y, 0))
    goto lab;
  u *= 2;
  if (__builtin_expect (z, 0))
    goto lab;
  u |= 42;
  if (__builtin_expect (w, 0))
    {
      lab:;
      bar ();
      bar ();
      if (bar () > 32) goto lab;
    }
  return u;
}
