// PR c++/101906
// Verify the template arguments of an alias template-id are evaluated even
// in an unevaluated context.
// { dg-do compile { target c++11 } }

template<int, class T> using skip = T;

template<class T>
constexpr unsigned sizeof_() {
  return sizeof(skip<(T(), 0), T>);
}

struct A {
  int m = -1;
};

static_assert(sizeof_<A>() == sizeof(A), "");
