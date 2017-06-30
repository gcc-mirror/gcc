// PR c++/81026

namespace std {
  template<class> struct extent;
}
using namespace std;

template <class T>
struct S {
  void f() { T().template extent<42>(); }
};
