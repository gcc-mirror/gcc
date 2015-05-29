// { dg-options "-fdump-tree-optimized -Os" }
#include <set>
#include <stdio.h>

int main()
{
  static const int array[] = { 1,2,3,4,5,6,7,8,9,10,6 };
  std::set<int> the_set;
  int count = 0;
  for (unsigned i = 0; i < sizeof(array)/sizeof(*array); i++)
  {
    std::pair<std::set<int>::iterator, bool> result =
      the_set.insert(array[i]);
    if (result.second)
      count++;
  }
  printf("%d unique items in array.\n", count);
  return 0;
}

// This function is small enough to be inlined even at -Os.
// { dg-final { scan-tree-dump-not "_ZNSt8_Rb_treeIiiSt9_IdentityIiESt4lessIiESaIiEED2Ev" "optimized" } }
