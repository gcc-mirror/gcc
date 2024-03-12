/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-dominator-opts -fno-tree-vrp -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre -fno-tree-copy-prop" } */
int f()
{
  int t = 0;
  return (t & 1) != 0;
}
