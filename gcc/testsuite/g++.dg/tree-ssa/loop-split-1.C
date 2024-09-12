/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-lsplit-details -std=c++11" } */
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>
#include <cmath>

constexpr unsigned s = 100000000;

int main()
{
    std::vector<float> a, b, c;
    a.reserve(s);
    b.reserve(s);
    c.reserve(s);

    for(unsigned i = 0; i < s; ++i)
    {
        if(i == 0)
            a[i] = b[i] * c[i];
        else
            a[i] = (b[i] + c[i]) * c[i-1] * std::log(i);
    }
}
/* { dg-final { scan-tree-dump-times "loop split" 1 "lsplit" } } */
