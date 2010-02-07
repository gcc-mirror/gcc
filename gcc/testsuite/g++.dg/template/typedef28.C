// Origin: PR c++/42820
// { dg-do compile }


template <class T> struct vector{};
struct Traits{struct Primitive{struct Id{};};};

template <class Tree, class Polyhedron> struct Tree_vs_naive
{
  typedef typename Tree::Primitive Primitive;

  void f() const
  {
	  typedef vector<typename Primitive::Id> Id_vector;
  }
};

template <class Tree> void test_hint_strategies()
{
  vector<typename Tree::Primitive::Id> v;
}

int main(void)
{
  test_hint_strategies<Traits>();
}


