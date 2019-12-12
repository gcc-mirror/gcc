/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

struct a {int array[3];} a[10];
int
test(int i,int j)
{
  a[i].array[1]=123;
  a[j].array[2]=2;
  return a[i].array[1];
}
/* { dg-final { scan-tree-dump-times "return 123" 1 "fre1"} } */
