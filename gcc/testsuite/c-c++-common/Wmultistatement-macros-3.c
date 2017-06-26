/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define CHECK(X) if (!(X)) __builtin_abort ()

void
fn (int i)
{
  CHECK (i == 1);
  CHECK (i == 2);
}
