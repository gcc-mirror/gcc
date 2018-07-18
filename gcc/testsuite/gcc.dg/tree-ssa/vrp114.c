/* { dg-do link { target int32plus } } */
/* { dg-options "-O2 -fdump-tree-fre1 -fdump-tree-evrp" } */

extern void link_error ();
void foo (int a)
{
  if (a < 0)
    {
      int y;
      a = -a;
      y  = a / 7;
      y = y * 2;
      if (y > 1 << 30)
	link_error ();
    }
}

int main()
{
  return 0;
}

/* { dg-final { scan-tree-dump-times "link_error" 1 "fre1" } } */
/* { dg-final { scan-tree-dump-times "link_error" 0 "evrp" } } */
