/* PR c/67784 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int T;

void
fn1 (void)
{
  if (sizeof (enum { T }) == 0)
    ;
  T x;
}

void
fn2 (void)
{
  int i = 0;
  if (sizeof (enum { T }) == 0)
    i++;
  T x;
}

void
fn3 (void)
{
  if (sizeof (enum { T }) == 0)
    {
    }
  T x;
}

void
fn4 (void)
{
  if (sizeof (enum { T }) == 0)
L:
    ;
  T x;
}

void
fn5 (void)
{
  if (sizeof (enum { T }) == 0)
    ;
  else
    ;
  T x;
}
