/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */

#include <vector>

void f4 (std::vector<int> a, std::vector<int> b, int n)
{
    int i = 0;
#pragma GCC novector
    while (i < (n & -8))
      {
        a[i] += b[i];
        i++;
      }
}

void f5 (std::vector<int> a, std::vector<int> b, int n)
{
    int i = 0;
#pragma GCC novector
#pragma GCC ivdep
#pragma GCC unroll 2
    while (i < (n & -8))
      {
        a[i] += b[i];
        i++;
      }
}

void f6 (std::vector<int> a, std::vector<int> b, int n)
{
    int i = 0;
#pragma GCC ivdep
#pragma GCC novector
#pragma GCC unroll 2
    while (i < (n & -8))
      {
        a[i] += b[i];
        i++;
      }
}

void f7 (std::vector<int> a, std::vector<int> b, int n)
{
    int i = 0;
#pragma GCC ivdep
#pragma GCC unroll 2
#pragma GCC novector
    while (i < (n & -8))
      {
        a[i] += b[i];
        i++;
      }
}

#if __cpp_range_based_for
void f8 (std::vector<int> a, std::vector<int> b, int n)
{
    int i = 0;
#pragma GCC novector
    for (int x : b)
      {
        a[i] += x;
        i++;
      }
}
#endif

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
