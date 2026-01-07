// PR c++/122752

struct set {
  typedef unsigned size_type;
  unsigned size() { return 42; }
};

template<class S>
struct A {
  typedef S VertexSet;
  typename VertexSet::size_type size();
  VertexSet vertices_;
};

template<class S>
inline typename A<S>::VertexSet::size_type A<S>::size()
{ return vertices_.size(); }

int main() {
  A<set> a;
  a.size();
}
