// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S { using type = S; int s; };
S a = { 1 }, b = { 2 };
constexpr S c[] = { { 3 }, { 4 }, { 5 }, { 6 }, { 7 } };
struct T { using type = T; int s; };
T d = { 8 };
struct U {
  constexpr const S *begin () const { return &c[0]; }
  constexpr const S *end () const { return &c[s]; }
  int s;
};
struct V { int a; long b; double c; };

void
foo ()
{
  template for (auto g : { a, b })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
  template for (auto g : { d, b })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
  static constexpr U u = { 3 };
  template for (auto g : u)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;
    }
  V v = { 9, 10L, 11.0 };
  template for (auto g : v)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
}

template <int N>
void
bar ()
{
  template for (auto g : { a, b })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
  template for (auto g : { d, b })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
  static constexpr U u = { 3 };
  template for (auto g : u)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;
    }
  V v = { 9, 10L, 11.0 };
  template for (auto g : v)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
}

template <typename S, typename U, typename V>
void
baz ()
{
  template for (auto g : { (S) a, b })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
  template for (auto g : { d, (S) b })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
  static constexpr U u = { 3 };
  template for (auto g : u)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
  V v = { 9, 10L, 11.0 };
  template for (auto g : v)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      decltype(g)::type h = g;		// { dg-error "need 'typename' before 'decltype \\\(g\\\)::type' because 'decltype \\\(g\\\)' is a dependent scope" }
    }
}

void
qux ()
{
  bar <0> ();
  baz <S, U, V> ();
}
