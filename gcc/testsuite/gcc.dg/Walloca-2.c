/* { dg-do compile } */
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
    p = __builtin_alloca (n); // { dg-warning "large due to conversion" }
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
      p = __builtin_alloca (n); // { dg-warning "'alloca' may be too large" "" { target lp64} }
      // { dg-message "note:.*argument may be as large as 2999" "note" { target lp64 } 38 }
      // { dg-warning "unbounded use of 'alloca'" "" { target { ! lp64 } } 38 }
    }
  else
    p = __builtin_malloc (n);
  f (p);
}
