/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

int main()
{
  while(1)
    for(int i=0; i<9000000; i++){}
}

/* { dg-final { scan-tree-dump-not "if" "cddce1" } } */
