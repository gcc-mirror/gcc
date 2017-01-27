/* PR c/79199 */
/* { dg-do compile { target int32plus } } */
/* { dg-options "-Wduplicated-branches" } */

unsigned int a, b, c, d, e;
void
fn1 (void)
{
  if (0) /* { dg-warning "this condition has identical branches" } */
    {
      if (d > 4294967293U)
	(void) 5;
      c = d;
      b = e | a;
    }
  else
    {
      if (d > 4294967293U)
	(void) 5;
      c = d;
      b = e | a;
    }
}
