// PR c++/99815
// { dg-do compile { target c++20 } }

template <class T, class U>
struct is_same { static constexpr bool value = false; };

template <class T>
struct is_same<T, T> { static constexpr bool value = true; };

template <class... Ts>
concept C = is_same<Ts...>::value; // { dg-error "wrong number" }

template <class... Ts> void f() {
  C<Ts...> auto x = 0; // { dg-error "constraints" }
}

template void f<int>(); // { dg-bogus "" }
template void f<char>(); // { dg-message "required from here" }
template void f<>(); // { dg-message "required from here" }
template void f<int, int>(); // { dg-message "required from here" }

template <class... Ts> void g() {
  C<Ts..., int> auto x = 0; // { dg-error "constraints" }
}

template void g<>(); // { dg-bogus "" }
template void g<int>(); // { dg-message "required from here" }

template <class> void h() {
  C<char> auto x = 0; // { dg-error "constraints" }
  C<int> auto y = 0;
}
