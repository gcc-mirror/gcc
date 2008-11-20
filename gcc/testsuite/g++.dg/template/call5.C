// PR c++/36631
// { dg-options "-O0" }

template <typename T> struct B
{
  struct C
  {
    __attribute__ ((always_inline)) C (C const &c) {}
  };
  void __attribute__ ((always_inline)) g (C c) {}
};

void
trigger (B <int> b, B <int>::C c)
{
  b.g (c);
}
