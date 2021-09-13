// { dg-do compile { target c++20 } }

template<class T>
struct A { static const bool value = T::value; };

template<class T>
void f() {
  // Verify we don't substitute into a lambda's constraints when
  // regenerating it, which would lead to a hard error here.
  [] () requires (T::value && A<T>::value) || true { }();
  [] <class U> (U) requires (U::value && A<T>::value) || true { }(0);
}

template void f<int>();
