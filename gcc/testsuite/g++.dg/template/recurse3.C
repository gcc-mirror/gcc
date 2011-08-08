// PR c++/44609
// { dg-options -ftemplate-depth=10 }

template<int N>
void f()
{
  0 = 0;			// { dg-error "lvalue required" }
  f<N+1>();			// { dg-bogus "instantiation depth" }
}

int main()
{
  f<0>();
}
