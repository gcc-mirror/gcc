// PR c++/95562
// { dg-do compile { target c++11 } }

template <bool Nothrow>
struct Functions
{
  void (*func)(void*) noexcept(Nothrow);
};

void test()
{
  Functions<true> f{};
}
