// { dg-do compile { target c++11 } }

template <class T> struct A
{
  T t;
  constexpr A() { }		// { dg-error "::t" }
};

int main()
{
  constexpr A<int> a;		// { dg-error "A()" }
}
