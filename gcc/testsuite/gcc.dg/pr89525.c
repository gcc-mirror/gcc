/* PR c/89525 */
/* { dg-do compile } */
/* { dg-options "-w" } */

double sqrt ();	/* { dg-bogus "declared here" } */

void
foo (void)
{
  sqrt ();
}
