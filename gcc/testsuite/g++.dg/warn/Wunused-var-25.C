// { dg-do compile { target c++14 } }
// { dg-options "-Wunused-but-set-variable" }

template <int... I> struct A { };
template <int... I>
auto f()
{
  constexpr int ar[sizeof...(I)+1] = {I...};
  return A<ar[I]...>();
}

int main()
{
  f<>();
}
