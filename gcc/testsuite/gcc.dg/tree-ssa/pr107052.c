// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp" }

void link_failure();
void f(int a)
{
  a &= 0x300;
  int b =  __builtin_popcount(a);
  if (b > 3)
    link_failure();
}

// { dg-final { scan-tree-dump-not "link_failure" "evrp" } }
