/* PR/71352 */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-reassoc1" } */

unsigned a, b, c, d, e;

void
fn1 ()
{
  unsigned f;
  e = f = d * -b + a * -c;
}

/* Check that we factor -1 and create -(d * b + a * c).  */
/* { dg-final { scan-tree-dump-times " = -" 1 "reassoc1" } } */
