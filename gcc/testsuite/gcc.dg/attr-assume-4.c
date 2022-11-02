/* PR tree-optimization/107369 */
/* { dg-do compile } */
/* { dg-options "-std=c2x -O1" } */

void
foo (int x)
{
  if (x == 1)
    goto l1;				/* { dg-error "jump into statement expression" } */

  [[gnu::assume (({ l1:; 1; }))]];	/* { dg-message "label 'l1' defined here" } */
}
