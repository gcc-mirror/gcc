/* PR c/93573 */
/* { dg-do compile } */
/* { dg-options "-std=gnu17" } */

void bar ();

void
foo (char a)
{
  union C { int d[100.0]; char *e; };	/* { dg-error "has non-integer type" } */
  bar ((union C) &a);
}
