/* { dg-do compile } */
/* { dg-options "-Walloca-larger-than=2000 -O2" } */

void f (void *);

void
g1 (int n)
{
  void *p;
  if (n > 0 && n < 2000)
    p = __builtin_alloca (n); // { dg-bogus "unbounded use of 'alloca'" "" }
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
