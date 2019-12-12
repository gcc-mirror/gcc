// PR c++/88612
// { dg-do compile { target c++11 } }
// { dg-options "-fpack-struct -Waddress-of-packed-member" }
// { dg-prune-output "taking address of packed member" }

template<class F, class... T>
auto indirect_call(F f, T... t) -> decltype(f(t...))
{
  return f(t...);
}

template<class F, class T>
struct VariadicBind
{
  F f;
  T t;

  template<class... A>
  auto operator()(A... a) -> decltype(indirect_call(f, t, a...))
  {
    return indirect_call(f, t, a...);
  }
};

template<class F>
void apply(F f)
{
  f();
}

template<class F, class V1, class... V>
void apply(F f, V1 v1, V... v)
{
  apply(VariadicBind<F, int>{f, v1}, v...);
}

void func(int, int) { }

int main()
{
  apply(func, 0, 0);
}
