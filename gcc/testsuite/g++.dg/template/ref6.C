// PR c++/53816

template <typename T>
struct S { int v () const; };
template <typename T>
struct V : public S<T> {};
struct U
{
  V<int> v;
  template<typename T>
  struct W
  {
    W (U const &x) { V<int> const &v = x.v; v.v(); }
  };
};
