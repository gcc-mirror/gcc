// PR c++/96410
// { dg-do compile { target c++20 } }

struct S { using blah = void; };

template <typename T> constexpr bool trait = !__is_same(T, S);
template <typename T> concept C = trait<T>;

template<typename T>
void foo() noexcept(!__is_same(T, void)) { }

template<typename U>
auto f() {
  return []<typename T>(T, bool a = requires { C<T>; }){ // { dg-warning Wmissing-requires }
    static_assert(requires { requires C<U> && (C<T> || C<T>); }); // { dg-error "assert" }
    static_assert(requires { C<T>; }); // { dg-warning Wmissing-requires }
    static_assert(requires { { foo<T>() } noexcept -> C; });
    static_assert(!requires { typename T::blah; }); // { dg-error "assert" }
    return 0;
  };
}

auto g = f<int>(); // { dg-bogus "" }
int n = g(0); // { dg-bogus "" }
int m = g(S{}); // { dg-message "required from here" }
