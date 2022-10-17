// PR c++/103401
// { dg-do compile { target c++23 } }

void f(decltype(auto(0))) { }

int main()
{
  f<int,int>(0); // { dg-error "no matching function" }
}
