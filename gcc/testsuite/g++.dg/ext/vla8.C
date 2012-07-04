// PR c++/42387
// { dg-options "" }

template<class PF>
struct AvlTreeIter
{
  int Num();

  AvlTreeIter()
  {
    new (void* [Num()]); // { dg-warning "variable-length array" }
  }
};

AvlTreeIter<int> a; // { dg-message "from here" }
