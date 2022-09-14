// PR c++/101906
// Like unevaluated1b.C, but using a variable template instead of a
// function template.
// { dg-do compile { target c++14 } }

template<int, class T> T skip;

template<class T>
constexpr unsigned sizeof_() {
  return sizeof(skip<(T(), 0), T>);
}

struct A {
  int m = -1;
};

static_assert(sizeof_<A>() == sizeof(A), "");
