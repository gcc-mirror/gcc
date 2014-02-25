// PR c++/60046
// { dg-require-effective-target c++11 }

constexpr bool foo () { return noexcept (true); }
template <typename T>
struct V
{
  void bar (V &) noexcept (foo ()) {}
};
template <typename T>
struct W : public V <int>
{
  void bar (W &x) { V <int>::bar (x); }
};

int
main ()
{
  W <int> a, b;
  a.bar (b);
}
