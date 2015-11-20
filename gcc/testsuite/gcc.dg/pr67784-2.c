/* PR c/67784 */
/* { dg-do compile } */
/* { dg-options "" } */

int T;

void
fn1 (void)
{
  for (typedef int T;;) /* { dg-error "declaration of non-variable" } */
    if (1)
      ;
  T *x; /* { dg-error "undeclared" } */
}

void
fn2 (void)
{
  for (typedef int T;;) /* { dg-error "declaration of non-variable" } */
    if (1)
      T = 1; /* { dg-error "expected expression" } */
  T *x; /* { dg-error "undeclared" } */
}

void
fn3 (void)
{
  for (typedef int T;;) /* { dg-error "declaration of non-variable" } */
    if (1)
      {
      }
  T *x; /* { dg-error "undeclared" } */
}

void
fn4 (void)
{
  for (typedef int T;;) /* { dg-error "declaration of non-variable" } */
    if (1)
L:
      ;
  T *x; /* { dg-error "undeclared" } */
}

void
fn5 (void)
{
  for (typedef int T;;) /* { dg-error "declaration of non-variable" } */
    if (1)
      ;
    else
      ;
  T *x; /* { dg-error "undeclared" } */
}
