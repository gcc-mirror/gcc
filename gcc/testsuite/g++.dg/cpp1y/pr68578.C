// { dg-do compile { target c++14 } }

template <typename> struct bar foo; template <> struct foo<>:  // { dg-error "class template|expected" }
