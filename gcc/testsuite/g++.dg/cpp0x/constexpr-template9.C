// PR c++/70543
// { dg-do compile { target c++11 } }

template <typename>
struct X
{
  template <unsigned int = 0>
  static constexpr int
  calc (void)
  {
    return 0;
  }

  static constexpr unsigned int value = calc ();

  char foo[value];
};
