// Build don't link:

template <class X, class Y>
struct Inner;

struct S
{
  template <class U>
  struct Inner
  {
  };
};


S::Inner<int> si;
