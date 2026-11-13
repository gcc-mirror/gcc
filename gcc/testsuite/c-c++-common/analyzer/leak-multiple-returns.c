/* { dg-additional-options "-fno-exceptions" } */

extern int cond (int);

void
multiple_returns (int a, int b, int c)
{
  void *p = __builtin_malloc (16);

  if (cond (a))
    return; /* { dg-warning "leak of 'p'" "leak at return after cond(a)" } */

  if (cond (b))
    return; /* { dg-warning "leak of 'p'" "leak at return after cond(b)" { xfail *-*-* } } */

  if (cond (c))
    return; /* { dg-warning "leak of 'p'" "leak at return after cond(c)" { xfail *-*-* } } */

  __builtin_free (p);
}
