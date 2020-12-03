/* PR tree-optimization/91029 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void kill (void);
int xx;

void f1 (int i)
{
  if ((i % 7) == 3)
    {
      xx = (i < 0);
      if (xx)
        kill ();
    }
}

void f2 (int i)
{
  if ((i % 7) > 0)
    {
      xx = (i < 0);
      if (xx)
        kill ();
    }
}

void f3 (int i)
{
  if ((i % 7) == -3)
    {
      xx = (i > 0);
      if (xx)
        kill ();
    }
}

void f4 (int i)
{
  if ((i % 7) < 0)
    {
      xx = (i > 0);
      if (xx)
        kill ();
    }
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } }  */
