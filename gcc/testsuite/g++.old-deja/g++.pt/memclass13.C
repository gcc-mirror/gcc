// Build don't link:

template <class X, class Y>
struct Inner;

template <class T>
struct S
{
  template <class U>
  struct Inner
  {
  };
};


S<double>::Inner<int> si;
