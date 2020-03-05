// PR c++/90966
// { dg-do compile { target c++11 } }

template<typename I>
void f()
{
  using S = signed char;
  constexpr const S v[]{0};
}

int main()
{
  f<int>();
}
