// PR c++/59268
// { dg-do compile { target c++11 } }

template <typename>
struct A
{
  constexpr A (int) {}
  virtual void foo ()
  {
    constexpr A<void> a (0);
  }
};

void
bar ()
{
  A<int> a (3);
  a.foo ();
}
