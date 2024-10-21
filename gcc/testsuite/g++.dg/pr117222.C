// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O3 -fdump-tree-evrp" }

#include <vector>
int main()
{
    std::vector<int> c {1,2,3,0};
    while(c.size() > 0 && c.back() == 0)
    {
        auto sz = c.size() -1;
        c.resize(sz);
    }
    return 0;
}
/* { dg-final { scan-tree-dump "Global Exported.*\[-INF, -1\]\[1, +INF\]" "evrp" } } */
