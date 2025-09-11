// { dg-do compile }
// { dg-additional-options "-mavx2" { target avx2 } }

#include <vector>

class Foo
{
public:
    void fun (std::vector<int>& blacksq);
    std::vector<int> m_mcowner;
};

void Foo::fun(std::vector<int>& blacksq)
{
  for (unsigned int i = 0; i < (unsigned int)blacksq.size(); i++)
    if (blacksq[i])
      m_mcowner[i]++;
}

// { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target avx2 } } }
