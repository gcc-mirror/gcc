/* PR 23196 */
/* { dg-options "-fforce-addr" } */

void foo()
{
  char c;

  c |= 1;
  bar(&c);
}
