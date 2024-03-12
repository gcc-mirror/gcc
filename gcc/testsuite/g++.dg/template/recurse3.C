// PR c++/44609
// { dg-options -ftemplate-depth=10 }

template<class T, int N>
void f()
{
  T(0) = 0;			// { dg-error "lvalue required" }
  f<T, N+1>();			// { dg-bogus "instantiation depth" }
}

int main()
{
  f<int, 0>();
}
