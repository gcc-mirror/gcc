/* Test ICE with variable-size struct initializer: bug 93577.  */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (int c)
{
  struct s
  {
    int a;
    int x[c];
    struct
    {
      int a, b;
    } nest;
  } v[2] = { [1].nest.b = 1 }; /* { dg-error "variable-sized object may not be initialized" } */
}
