// PR c++/116568

template <typename> struct S {
  template <typename> using t = decltype([]{});
};

// 't' does not currently have a mangling scope, but should not ICE
using t = S<int>::t<int>;
