/* PR tree-optimization/68455 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int r;
int n;

void
fn1 (void)
{
  int i;

  for (i = 0; i < 1; ++i)
    {
      unsigned short int u;
      if (u < n)
	r = 1 / n;
    }
}
