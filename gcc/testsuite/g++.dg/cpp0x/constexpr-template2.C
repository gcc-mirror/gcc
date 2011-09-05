// { dg-options -std=c++0x }

template <class T> struct A
{
  T t;
  constexpr A() { }		// { dg-error "uninitialized" }
};

int main()
{
  constexpr A<int> a;		// { dg-error "A()" }
}
