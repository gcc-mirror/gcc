// { dg-options "-std=c++0x" }
template<typename... T> int foo()
{
  typename T::X x; // { dg-error "parameter packs|T" }
  return x;
}

void bar()
{
  foo<int>();
}
