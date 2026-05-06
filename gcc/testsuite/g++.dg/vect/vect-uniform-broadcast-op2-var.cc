/* Gather load with PLUS (hi, lo).  The second operand of the offset PLUS is
   a variable, hence the offset is not uniform and no broadcast is generated.
   The inner loop still vectorizes as a normal gather.  The conditional store
   needs a masked store, so enable AVX2.  */
/* { dg-do compile { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-mavx2 -mprefer-vector-width=128" } */

#include <vector>

class Foo
{
public:
  void fun (unsigned boardsize);
  std::vector<int> m_mcowner;
};

void
Foo::fun (unsigned boardsize)
{
  unsigned nwords = (21 * 21 + 63) / 64 + 2;
  std::vector<unsigned long> words (nwords, 0);
  __asm__ ("" : "+g" (words));
  for (unsigned i = 0; i < boardsize; i++)
    if (i % 2 == 0)
      words[i / 64] |= 1UL << (i % 64);
  unsigned int i = 0;
  do
    {
      unsigned hi = i / 64;
      unsigned lo = i & 1u;
      /* The second operand is loop-varying, so the offset is not a
	 shift, division or AND of the IV.  */
      if (words[hi + lo])
	m_mcowner[i]++;
    }
  while (++i < 21 * 21);
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* The emulated gather extracts each lane of the offset vector.  */
/* { dg-final { scan-tree-dump "BIT_FIELD_REF <vect_" "vect" } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
