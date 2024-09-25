/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */

#include <vector>

#define N 10

static void visit(int &level, int n, int k, std::vector< int > &value) {
  level = level + 1;
  value[k] = level;
  for (int i = 0 ; i < n; i++)
    if (value[i] == 0)
      visit(level, n, i, value);
}
void permutations()
{
  std::vector< int > value(N);
  int level = -1;
  visit(level, N, 0, value);
}
void testExtendByBox() {
  permutations();
}

int main() {
  testExtendByBox();
  return 0;
}
