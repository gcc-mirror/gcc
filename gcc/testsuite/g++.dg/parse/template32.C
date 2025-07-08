// PR c++/119838
// { dg-do compile { target c++11 } }

template<typename T>
struct S { using U = T; static const int x = 0; };
void
g ()
{
  ::S<int>::U a;
  ::template S<int>::U b;
  auto c = ::S<int>::x;
  auto d = ::template S<int>::x;
}
