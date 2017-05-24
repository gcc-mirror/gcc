/* PR tree-optimization/80558 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */
/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */

void link_error (void);

void
f1 (int x)
{
  if (x >= 5 && x <= 19)
    {
      x &= -2;
      if (x < 4 || x > 18)
	link_error ();
    }
}

void
f2 (int x)
{
  if (x >= 5 && x <= 19)
    {
      x |= 7;
      if (x < 7 || x > 23)
	link_error ();
    }
}

void
f3 (int x)
{
  if (x >= -18 && x <= 19)
    {
      x |= 7;
      if (x < -17 || x > 23)
	link_error ();
    }
}

void
f4 (int x)
{
  if (x >= 1603 && x <= 2015)
    {
      x &= 496;
      if (x < 64 || x > 464)
	link_error ();
    }
}
