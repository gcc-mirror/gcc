// { dg-do compile { target c++2a } }

template<typename T>
concept integer = __is_same_as(T, int);

template<typename T>
concept subst = requires (T x) { requires true; };

template<typename T>
concept c1 = requires { requires integer<T> || subst<T&>; }; // { dg-message "in requirements" }

static_assert(requires { requires true; });
static_assert(requires { requires false; }); // { dg-error "static assertion failed" }
static_assert(requires { requires integer<int>; });
static_assert(requires { requires integer<void>; }); // { dg-error "static assertion failed" }
static_assert(requires { requires c1<int>; });
static_assert(requires { requires c1<bool>; });
static_assert(requires { requires c1<void>; }); // { dg-error "static assertion failed" }
static_assert(requires { requires subst<void&>; }); // { dg-error "cannot declare|failed" }

static_assert(c1<int>);
static_assert(c1<bool>);
static_assert(c1<void>); // { dg-error "static assertion failed" }

template<c1 T>
void f1() { }

template<typename T>
  requires requires { requires integer<T> || subst<T&>; } // { dg-message "in requirements" }
void f2();

template<typename T>
struct data
{
  template<c1 U>
  void f1() {}

  template<typename U>
    requires requires { requires integer<U> || subst<U&>; } // { dg-message in requirements" }
  void f2() {}

  static_assert(requires { requires subst<T&>; }); // { dg-error "forming reference|failed" }

  template<typename U>
  constexpr bool test()
  {
    if constexpr (requires { requires subst<U&>; })
      return true;
    else
      return false;
  }
};

template<typename T>
constexpr bool check_for_resize(T &v, unsigned const n)
{
  if constexpr (requires { v.resize(n); })
    return true;
  else
    return false;
}

struct array { };
struct vector { void resize(int n); };

void test()
{
  f1<int>();
  f1<bool>();
  f1<void>(); // { dg-error "unsatisfied" }

  f2<int>();
  f2<bool>();
  f2<void>(); // { dg-error "unsatisfied" }

  data<char> x;
  x.f1<int>();
  x.f1<bool>();
  x.f1<void>(); // { dg-error "no matching function" }
  x.f2<int>();
  x.f2<bool>();
  x.f2<void>(); // { dg-error "no matching function" }

  data<void> fail;

  data<int> t;
  static_assert(t.test<int>());
  static_assert(t.test<void>()); // { dg-error "static assertion failed" }

  vector v;
  static_assert(check_for_resize(v, 10));
  array a;
  static_assert(!check_for_resize(a, 10));
}
