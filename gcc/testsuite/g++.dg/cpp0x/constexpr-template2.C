// { dg-options -std=c++11 }

template <class T> struct A
{
  T t;
  constexpr A() { }		// { dg-error "uninitialized" }
};

int main()
{
  constexpr A<int> a;		// { dg-error "A()" }
}
