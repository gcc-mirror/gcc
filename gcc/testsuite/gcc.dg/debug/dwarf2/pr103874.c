/* PR debug/103874 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -gsplit-dwarf -dA -Wno-implicit-function-declaration" } */

void
foo (void)
{
  {
    bar ();
    baz ();
  }
}
