/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

int a;
int b[3];
void test()
{
  if (a > 0)
    {
      b[0] = 0;
      b[1] = 1;
      b[2] = 2;
      __builtin_unreachable ();
    }
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 3 "dse1" } } */
