// { dg-options "-std=c++0x" }

template<class U, class... T>
void f()
{
  f<T...>(); // { dg-error "no matching" }
}

template<>
void f() { } // { dg-error "template-id" }

int main()
{
  f<char>();
}
