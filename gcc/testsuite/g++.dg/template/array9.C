// PR c++/18429

int subtrees = 4;
template< class T >
struct Tree {
  Tree* L[subtrees]; // { dg-error "" }
  Tree* R[subtrees]; // { dg-error "" }
  ~Tree()
  {
    delete [] L[0]; // { dg-error "" }
    delete [] R[0]; // { dg-error "" }
  }
};

void f()
{
  Tree<int> t;
}
