/* PR c/60195 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wpedantic -Wall" } */

_Atomic int a;

void
fn (void)
{
  ++a;
  a++;
  --a;
  a--;
}
