/* PR c/67784 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int T;

void
fn1 (void)
{
  for (int T;;)
    if (1)
      ;
  T *x;
}

void
fn2 (void)
{
  for (int T;;)
    if (1)
      T = 1;
  T *x;
}

void
fn3 (void)
{
  for (int T;;)
    if (1)
      {
      }
  T *x;
}

void
fn4 (void)
{
  for (int T;;)
    if (1)
L:
      ;
  T *x;
}

void
fn5 (void)
{
  for (int T;;)
    if (1)
      ;
    else
      ;
  T *x;
}
