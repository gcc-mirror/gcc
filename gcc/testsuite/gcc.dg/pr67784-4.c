/* PR c/67784 */
/* { dg-do compile } */
/* { dg-options "-Wno-switch-unreachable" } */

typedef int T;

void
fn1 (void)
{
  switch (sizeof (enum { T }))
    if (1)
      ;
  T x;
}

void
fn2 (void)
{
  int i = 0;
  switch (sizeof (enum { T }))
    if (1)
      i++;
  T x;
}

void
fn3 (void)
{
  switch (sizeof (enum { T }))
    if (1)
      {
      }
  T x;
}

void
fn4 (void)
{
  switch (sizeof (enum { T }))
    if (1)
L:
      ;
  T x;
}

void
fn5 (void)
{
  switch (sizeof (enum { T }))
    if (1)
      ;
    else
      ;
  T x;
}
