/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=2000 -O2" } */

void f (void *);

void
g1 (int n)
{
  void *p;
  if (n > 0 && n < 2000)
    // FIXME: This is a bogus warning, and is currently happening on
    // 32-bit targets because VRP is not giving us any range info for
    // the argument to __builtin_alloca.  This should be fixed by the
    // upcoming range work.
    p = __builtin_alloca (n); // { dg-bogus "unbounded use of 'alloca'" "" { xfail { ! lp64 } } }
  else
    p = __builtin_malloc (n);
  f (p);
}

void
g2 (int n)
{
  void *p;
  if (n < 2000)
    p = __builtin_alloca (n); // { dg-warning "may be too large" }
  else
    p = __builtin_malloc (n);
  f (p);
}

void
g3 (int n)
{
  void *p;
  if (n > 0 && n < 3000)
    {
      p = __builtin_alloca (n); // { dg-warning "may be too large" }
    }
  else
    p = __builtin_malloc (n);
  f (p);
}
