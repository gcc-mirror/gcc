// { dg-do compile }
// { dg-options "-fgnu-tm -O0" }

/* Test that we generate transactional clones for both the base and
   the complete dtor for class Itemset.  */

class Itemset {
public:
   __attribute__((transaction_safe)) ~Itemset();
  __attribute__((transaction_safe)) void operator delete(void *);
private:
};

__attribute__((transaction_safe))
Itemset::~Itemset()
{
}

// { dg-final { scan-assembler "_ZGTtN7ItemsetD1Ev" } }
// { dg-final { scan-assembler "_ZGTtN7ItemsetD2Ev" } }
