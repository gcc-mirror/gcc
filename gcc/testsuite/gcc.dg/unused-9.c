/* PR c/98260 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

void g(void)
{
  int i = 0;		/* { dg-warning "variable 'i' set but not used" } */
  volatile int x;	/* { dg-bogus "variable 'x' set but not used" } */
  (x, i++);
}
