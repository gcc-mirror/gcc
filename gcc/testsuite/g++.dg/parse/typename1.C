template <class baz>
struct bar
{
  typedef typename baz::typename rebind<int> foo; // { dg-error "" }
};
