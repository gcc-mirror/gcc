// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
struct S1 {};

template<typename T>
concept bool C() { return requires(T x) { { x.fn() } -> S1<T>; }; }

template<C U>
void fn(U x)
{
  x.fn();
}

struct S2
{
  auto fn() const { return S1<S2>(); }
};

int main()
{
  fn(S2{});
  return 0;
}
