/* Test ICE with variable-size struct initializer: bug 93577.  */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (int c)
{
  struct s
  {
    int x[c];
    struct
    {
      int z;
    } nest;
  } v = { 1, 2 }; /* { dg-error "variable-sized object may not be initialized" } */
}
