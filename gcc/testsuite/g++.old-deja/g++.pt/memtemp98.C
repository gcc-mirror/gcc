// Build don't link:
//Purpose:
//  Test nested template as template template arg.
//Result:
//  Before the patch, got:
//   `C' is not a template

template <template <typename S> class T>
struct A
{
  T<int> m_t;
};

struct B
{
  template <typename V>
  struct C
  {
    V m_v;
  };
};

A<B::C> z;
