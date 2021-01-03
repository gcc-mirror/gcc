/* PR tree-optimization/97979 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp" } */

short a = 0;
int b = 0;

void
foo (void)
{
  unsigned short d = b;
  a = d >> -2U;	/* { dg-warning "right shift count >= width of type" } */
}
