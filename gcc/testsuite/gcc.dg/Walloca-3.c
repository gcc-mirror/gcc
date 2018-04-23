/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=2000 -O2" } */

void f (void *);

__SIZE_TYPE__ LIMIT;

// Warn when there is an alloca bound, but it is an unknown bound.

void
g1 (__SIZE_TYPE__ n)
{
  void *p;
  if (n < LIMIT)
    p = __builtin_alloca (n); // { dg-warning "may be too large" }
  else
    p = __builtin_malloc (n);
  f (p);
}

// Similar to the above, but do not get confused by the upcast.

unsigned short SHORT_LIMIT;
void
g2 (unsigned short n)
{
  void *p;
  if (n < SHORT_LIMIT)
    p = __builtin_alloca (n); // { dg-warning "may be too large" }
  else
    p = __builtin_malloc (n);
  f (p);
}
