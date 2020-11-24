/* PR tree-optimization/91029 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void kill (void);
int xx;

void f1 (int i, int j)
{
  if ((i % j) == 3)
    {
      xx = (i < 3);
      if (xx)
        kill ();
    }
}

void f2 (int i, int j)
{
  if ((i % j) > 0)
    {
      xx = (i <= 0);
      if (xx)
        kill ();
    }
}

void f3 (int i, int j)
{
  if ((i % j) == -3)
    {
      xx = (i > -3);
      if (xx)
        kill ();
    }
}

void f4 (int i, int j)
{
  if ((i % j) < 0)
    {
      xx = (i >= 0);
      if (xx)
        kill ();
    }
}

void f5 (int i, int j)
{
  if ((i % j) > 42)
    {
      xx = (i <= 42);
      if (xx)
        kill ();
    }
}

void f6 (int i, int j)
{
  if ((i % j) < -124)
    {
      xx = (i >= -124);
      if (xx)
        kill ();
    }
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } }  */
