// PR c++/105803
// { dg-do compile }
// { dg-additional-options "-fchecking=2" }

namespace std {
template <typename> class shared_ptr {};
}
struct S {};
template <int> void build_matrices() {
  std::shared_ptr<S>(new S);
}
