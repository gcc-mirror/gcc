/* Test case for generating a scalar load and broadcast instead of a gather
   when the gather offset is uniform across the vector lanes.  The
   conditional store needs a masked store, so enable AVX2.  */
/* { dg-do compile { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-mavx2 -mprefer-vector-width=128" } */

#include <bitset>
#include <vector>

class Foo
{
public:
  void fun (unsigned boardsize, bool check);
  std::vector<int> m_mcowner;
};

void
Foo::fun (unsigned boardsize, bool check)
{
  std::bitset<21*21> blacksq;
  __asm__ ("" : "+g" (blacksq));
  for (int i = 0; i < boardsize; i++)
    if (i % 2 == 0)
      blacksq[i] = true;
  unsigned int i = 0;
  do
    {
      if (blacksq[i])
	m_mcowner[i]++;
    }
  while (++i < blacksq.size ());
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* { dg-final { scan-tree-dump "computing cost for broadcast" "vect" } } */
