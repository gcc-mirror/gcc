// PR c++/92838
// { dg-do compile { target c++20 } }

template<int N>
auto foo()
{
  [] () requires (N != 0) { }(); // { dg-error "no match" }
  [] () requires (N == 0) { }();

  [] <int M=1> () requires (N == M) { }(); // { dg-error "no match" }
  [] <int M=1> () requires (N != M) { }();
}

void bar()
{
  foo<0>();
}
