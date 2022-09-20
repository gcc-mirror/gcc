// Verify a non-constant argument to __integer_pack respects SFINAE.
// { dg-do compile { target c++11 } }

template<int...> struct A { };

template<class T> auto f(int) -> A<__integer_pack(T::value)...> = delete;
template<class T> void f(...);

struct B { static int value; };

int main() {
  f<B>(0);
}
