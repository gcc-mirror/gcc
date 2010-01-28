// Origin: PR c++/42820
// { dg-do compile }

template <class T> struct vector{};
template<class T>struct Traits{struct Primitive{struct Id{};};};

template <template<class T> class Tree, class Polyhedron> struct Tree_vs_naive
{
  typedef typename Tree<int>::Primitive Primitive;

  void f() const
  {
    typedef vector<typename Primitive::Id> Id_vector;
  }
};

template <template<class T> class Tree> void test_hint_strategies()
{
  vector<typename Tree<int>::Primitive::Id> v;
}

int main(void)
{
  test_hint_strategies<Traits>();
}
