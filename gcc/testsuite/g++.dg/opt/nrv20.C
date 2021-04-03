// PR c++/91217
// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not "<retval> = a" "gimple" } }

struct A
{
  int ar[42];
};

template <class T>
A f()
{
  return [] { A a; return a; }();
}

int main()
{
  f<int>();
}
