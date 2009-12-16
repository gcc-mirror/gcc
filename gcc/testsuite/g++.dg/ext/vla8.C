// PR c++/42387
// { dg-options "" }

template<class PF>
struct AvlTreeIter
{
  int Num();

  AvlTreeIter()
  {
    new (void* [Num()]);
  }
};

AvlTreeIter<int> a;
