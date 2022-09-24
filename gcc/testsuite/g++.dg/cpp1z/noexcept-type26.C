// Verify a non-constant conditional noexcept-specifier in a function type
// respects SFINAE.
// { dg-do compile { target c++17 } }

template<class T> void f(void() noexcept(T::value)) = delete;
template<class T> void f(...);

struct B { static bool value; };

int main() {
  f<B>(nullptr);
}
