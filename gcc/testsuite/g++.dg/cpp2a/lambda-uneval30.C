// PR c++/124397
// { dg-do compile { target c++20 } }

template <class T>
auto f(T) -> decltype([]() { T::invalid; } ()); // { dg-error "'invalid' is not a member of 'int'" }
template<typename T>
void f(T, ...);

void d() {
  f(0); // { dg-error "ambiguous" }
}
