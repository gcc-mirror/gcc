/* PR middle-end/40943 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

void
foo (void)
{
  int *p;
  *p = 3;	/* { dg-warning "is used uninitialized" } */
}
