/* PR c/41842 */
/* { dg-do compile } */

void
f ()
{
  char x[g (h)];	/* { dg-error "undeclared|for each function" } */
}
