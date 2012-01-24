/* PR target/51957 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

#include "pr51957-1.h"

union R *w[10];

union R *
fn1 (void)
{
  return (union R *) 0;
}

void
fn2 (int x, const char *y, union R *z)
{
}

void
fn3 (void)
{
}

int
fn4 (union R *x)
{
  return 0;
}

int
main ()
{
  return 0;
}
