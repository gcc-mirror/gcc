/* PR c/98260 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */


void g(void)
{
  int i = 0;
  volatile int x;
  (x, i++);	/* { dg-bogus "set but not used" } */
}


