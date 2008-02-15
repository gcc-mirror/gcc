// { dg-options "-std=c++0x" }
template<typename... T> int foo()
{
  T t; // { dg-error "parameter packs|T" }
  return t;
}

void bar()
{
  foo<int>();
}
