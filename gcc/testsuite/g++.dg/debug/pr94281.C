// PR debug/94281
// { dg-do compile }
// { dg-options "-O1 -fno-tree-dce -fipa-icf -fno-tree-forwprop -fcompare-debug" }

void fn1()
{
}
void fn2()
{
  ;
}
