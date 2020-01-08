// { dg-do compile { target c++14 } }

template <class T> T var = T();

template <class T>
void f()
{
  constexpr T i = var<T>;	// { dg-error "19:var" }
}

int main()
{
  f<int>();
}
