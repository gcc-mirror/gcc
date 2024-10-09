// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++17 } }
// { dg-options "" }

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

template<int I, typename... Ts>
using Type = Ts...[I]; // { dg-warning "pack indexing only available with" "" { target c++23_down } }

template<int I, auto... Ts>
constexpr auto Var = Ts...[I]; // { dg-warning "pack indexing only available with" "" { target c++23_down } }

template <int I, auto...Ts>
int
foo ()
{
  return Ts...[I]; // { dg-warning "pack indexing only available with" "" { target c++23_down } }
}

template<typename... Ts>
struct S {
  Ts...[0] a; // { dg-warning "pack indexing only available with" "" { target c++23_down } }
#if __cpp_concepts >= 201907L
  void foo (auto... Vs) {
    decltype(Vs...[1]) d1 = Vs...[1]; // { dg-warning "pack indexing only available with" "" { target { c++20 && c++23_down } } }
  }
#endif
};

int
g ()
{
  using U = Type<1, char, int, float>;
  using U = int;

  constexpr auto V = Var<2, 0, 1, 42>;
  static_assert (V == 42);

  U r = foo<2, 0, 1, 42>();

  return r;
}

void
fn1 ()
{
  int i = 0;
  [&i](auto... pack) {
    // type is int
    decltype(pack...[0]) x5 = 42; // { dg-warning "pack indexing only available with" "" { target c++23_down } }
    // type is int&
    [[maybe_unused]] decltype((pack...[0])) x6 = i; // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  }(0);
}

#if __cpp_concepts >= 201907L
int
bar (auto... pack)
{
  (void) pack...[0]; // { dg-warning "pack indexing only available with" "" { target { c++20 && c++23_down } } }
  int x = pack...[0]; // { dg-warning "pack indexing only available with" "" { target { c++20 && c++23_down } } }
  return x;
}
#endif

template<auto...pack>
void
fn2 ()
{
  [[maybe_unused]] decltype(pack...[0]) x1; // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  [[maybe_unused]] decltype((pack...[0])) x2; // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  same_type<decltype(x1), int>();
  same_type<decltype(x2), int>();
}

template<typename... T>
void
fn3 (int p)
{
  T...[0] a = p; // { dg-warning "pack indexing only available with" "" { target c++23_down } }
  (T...[0])(a);  // { dg-warning "pack indexing only available with" "" { target c++23_down } }
}

template<int... Is>
void fn4 ()
{
  same_type<decltype(Is...[0]), int>();	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
  same_type<decltype((Is...[0])), int>(); // { dg-warning "pack indexing only available with" "" { target c++23_down } }
}

void
g3 ()
{
  fn2<0>();
#if __cpp_concepts >= 201907L
  bar (0);
#endif
  S<int> s;
  fn4<0, 1, 2>();
}
