// PR debug/94281
// { dg-do compile }
// { dg-options "-O1 -fno-tree-dce -fipa-icf -fno-tree-forwprop -fcompare-debug" }
// { dg-xfail-if "AIX compare debug" { powerpc-ibm-aix* } }

void fn1()
{
}
void fn2()
{
  ;
}
