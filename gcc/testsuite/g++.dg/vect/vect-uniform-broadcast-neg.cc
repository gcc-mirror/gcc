/* Negative test: the loop starts at i = 1, so the SCEV of i is {1, +, 1}.
   A non-zero base B is only accepted when B is a multiple of the number of
   vector lanes, and 1 is not.  A vector chunk can then span two bitset
   words (e.g. i = 61..64), so the word offset i / 64 is not uniform across
   the lanes and no broadcast must be generated.  The conditional store
   needs a masked store, so enable AVX2.  */
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
  unsigned int i = 1;
  do
    {
      if (blacksq[i])
	m_mcowner[i]++;
    }
  while (++i < blacksq.size ());
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
