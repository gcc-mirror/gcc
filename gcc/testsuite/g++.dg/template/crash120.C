// PR c++/60493

template <class T, class U>
struct foo
{
};
template <class T>
struct baz
{
  class bar;
};

template <class T, class D>
struct baz<T>::bar : foo<int, D>  // { dg-error "parameters|required" }
{
};

baz<int>::bar it; // { dg-error "incomplete" }
