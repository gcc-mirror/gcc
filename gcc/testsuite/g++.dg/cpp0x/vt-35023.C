// { dg-do compile { target c++11 } }
template<typename... T> int foo()
{
  T t; // { dg-error "parameter packs|T" }
  return t;
}

void bar()
{
  foo<int>();
}
