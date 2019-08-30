/* PR other/89342 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__((optimize("Ofast")))
void foo (void)
{
  __attribute__((optimize("no-inline")))
  void bar (void) {}
  bar ();
}
