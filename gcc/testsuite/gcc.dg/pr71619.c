/* PR 71619 */

/* { dg-do compile } */
/* { dg-options "-O --param=max-predicted-iterations=0" } */

void
foo ()
{
  int count = -10;
  while (count++);
}
