/* PR c/68320 */
/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

void
fn1 (void)
{
  for (typedef int T;;)
    if (1)
      ;
  T x; /* { dg-error "unknown type name" } */
}

void
fn2 (int i)
{
  for (typedef int T;;)
    if (1)
      i = 5;
  T x; /* { dg-error "unknown type name" } */
}

void
fn3 (void)
{
  for (typedef int T;;)
    if (1)
      {
      }
  T *x; /* { dg-error "unknown type name" } */
}

void
fn4 (void)
{
  for (typedef int T;;)
    if (1)
      ;
  T, T; /* { dg-error "undeclared" } */
}

void
fn5 (void)
{
  for (typedef int T;;)
    if (1)
      ;
  T = 10; /* { dg-error "undeclared" } */
}

void
fn6 (void)
{
  for (typedef int T;;)
    if (1)
      ;
  T[0]; /* { dg-error "undeclared" } */
}

void
fn7 (void)
{
  for (typedef int T;;)
    if (1)
      ;
  T (); /* { dg-warning "implicit declaration" } */
}
