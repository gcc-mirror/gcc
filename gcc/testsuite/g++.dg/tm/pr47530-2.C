// { dg-do compile }
// { dg-options "-fgnu-tm -O2 -fno-inline -fdump-tree-tmedge" }

class RBTree
{
    struct RBNode
    {
      RBNode* next;
    };

  public:
    RBNode* sentinel;
    __attribute__((transaction_safe)) bool lookup();
};

bool RBTree::lookup()
{
  RBNode* x = sentinel;
  while (x)
    x = x->next;
  return false;
}


RBTree* SET;

void bench_test()
{
  __transaction_atomic { 
      SET->lookup();
    }
}

// { dg-final { scan-tree-dump-times "ITM_commitTransaction.*tail call" 0 "tmedge" } }
// { dg-final { cleanup-tree-dump "tmedge" } }
