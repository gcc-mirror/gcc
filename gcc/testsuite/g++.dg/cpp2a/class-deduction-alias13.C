// PR c++/105655
// { dg-do compile { target c++20 } }

template <class T>
struct A
{
  template <class L, class R>
  struct B
  {
    B(const L & left, const R & right)
    {}
  };

  template <class L, class R>
  B(const L &, const R &) -> B<L, R>;
};

template <class L, class R>
using C = A<int>::B<L, R>;

int main()
{
  C x{0, 0};
}
