/* PR tree-optimization/91029 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void kill (void);
int xx;

void f1 (int i, int j)
{
  if ((i % j) == 3)
    {
      xx = (j <= 3 && j >= -3);
      if (xx)
        kill ();
    }
}

void f2 (int i, int j)
{
  if ((i % j) > 0)
    {
      xx = (j <= 1 && j >= -1);
      if (xx)
        kill ();
    }
}

void f3 (int i, int j)
{
  if ((i % j) == -3)
    {
      xx = (j <= 3 && j >= -3);
      if (xx)
        kill ();
    }
}

void f4 (int i, int j)
{
  if ((i % j) < 0)
    {
      xx = (j <= 1 && j >= -1);
      if (xx)
        kill ();
    }
}

void f5 (int i, int j)
{
  if ((i % j) > 42)
    {
      xx = (j <= 43 && j >= -43);
      if (xx)
        kill ();
    }
}

void f6 (int i, int j)
{
  if ((i % j) < -124)
    {
      xx = (j <= 125 && j >= -125);
      if (xx)
        kill ();
    }
}

void f7 (unsigned int i, unsigned int j)
{
  if ((i % j) == 3)
    {
      xx = (j <= 3);
      if (xx)
        kill ();
    }
}

void f8 (unsigned int i, unsigned int j)
{
  if ((i % j) > 0)
    {
      xx = (j <= 1);
      if (xx)
        kill ();
    }
}

void f9 (unsigned int i, unsigned int j)
{
  if ((i % j) >= 124)
    {
      xx = (j <= 124);
      if (xx)
        kill ();
    }
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } }  */
