/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int main(int argc, char* argv[])
{
  int i, a = 0;
  for (i=0; i < 10; i++)
    a += i + 0xff00ff;
  return a;
}

/* There should be no loop left.  */

/* { dg-final { scan-tree-dump-times "goto" 0 "optimized" } } */
