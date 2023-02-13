/* PR c++/105593 */
/* { dg-do compile } */
/* { dg-options "-W -Wall -Wno-init-self" } */

void bar (int);

static inline int
baz (void)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Winit-self"
  int u = u;		/* { dg-bogus "'u' is used uninitialized" } */
#pragma GCC diagnostic pop
  return u;
}

void
foo (void)
{
  int u = baz ();
  bar (u);
}

static inline int
qux (void)
{
  int u = u;		/* { dg-bogus "'u' is used uninitialized" } */
  return u;
}

void
corge (void)
{
  int u = qux ();
  bar (u);
}
