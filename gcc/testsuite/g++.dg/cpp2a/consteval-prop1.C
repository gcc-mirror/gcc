// P2564R3
// { dg-do compile { target c++20 } }
// Some of these were cribbed from clang's cxx2b-consteval-propagate.cpp.

consteval int id(int i) { return i; }

template <typename T>
constexpr int
f0 (T t)
{
  // OK, f0<int> promoted to consteval.
  return id (t); // { dg-message "immediate-escalating expression .id\\(t\\)." }
}

constexpr auto a0 = f0 (3);

// As a consequence of f0<int> being promoted to an immediate function, we
// can't take its address.
auto p0 = &f0<int>; // { dg-error "taking address of an immediate function" }

template <typename T>
constexpr int
f1 (T t)
{
  // OK, f1<int> promoted to consteval.
  return t + id (t); // { dg-message "immediate-escalating expression .id\\(t\\)." }
}

constexpr auto a1 = f1 (3);

// As a consequence of f1<int> being promoted to an immediate function, we
// can't take its address.
auto p1 = &f1<int>; // { dg-error "taking address of an immediate function" }

template <typename T>
constexpr int
f2 (T)
{
  // This produces a constant; f2 *not* promoted to consteval.
  return id (42);
}

// ... so we can take its address.
auto p2 = &f2<int>;

constexpr int
f3 (int i)
{
  // f3 isn't a function template and those don't get upgraded to consteval.
  return id (i); // { dg-error "not a constant expression" }
}

auto p3 = &f3;

template<typename T>
constexpr int
f4 (T t)
{
  auto p = id; // { dg-message "immediate-escalating expression .id." }
  (void) p;
  return t;
}

auto p6 = &f4<int>; // { dg-error "taking address of an immediate function" }

static_assert (f4 (42) == 42);

// Constructors.
consteval int zero (int)
{
  return 0;
}

struct A {
  // A::A(auto) promoted to consteval.
  constexpr A(auto i) { zero (i); }
};

constexpr void
f5 (auto i)
{
  A a{i};
}

constexpr void
f5_nt (int i)
{
  A a{i}; // { dg-error "call to consteval function|not a constant" }
}

void
f6 ()
{
  f5 (0);
}

struct B {
  constexpr B(int) { }
};

B b1(f0<int>((f1<int>(7))));

template<typename T>
constexpr int cid(T t) { return t; }

auto p4 = &cid<int>;
auto p5 = &cid<char>;

int g = 7; // { dg-message ".int g. is not const" }

B b2(f0<int>(cid<int>(g))); // { dg-error "call to consteval function|not usable" }

struct C {
  consteval C (int) {};
};

constexpr int
f7 (auto t)
{
  C c(t); // { dg-message "immediate-escalating expression .c.C::C\\(t\\)." }
  return 0;
}

int i1 = f7 (g); // { dg-error "call to consteval function|not usable" }

struct Y {
  int y;
  int x = id (y);
  consteval Y (int i) : y (id (i)) {}
};

Y y1(1);
Y y2(g); // { dg-error "call to consteval function|not usable" }

struct Y2 {
  int y;
  int x = id (y);
  constexpr Y2 (auto i) : y (id (i)) {}
};

Y2 y3(1);
Y2 y4(g); // { dg-error "call to consteval function|not usable" }

auto l1 = [](int i) constexpr {
  int t = id (i);
  return id (0);
};

int (*pl1)(int) = l1; // { dg-error "call to consteval function|returns address of immediate function" }

auto l2 = [](int i) {
  int t = id (i);
  return id (0);
};

int (*pl2)(int) = l2; // { dg-error "call to consteval function|returns address of immediate function" }

// Not defined = won't produce a constant expression.
consteval int undef (); // { dg-warning "used but never defined" }

struct S {
  int a = [] { return undef (); }();
};

struct S2 {  // { dg-error "used before its definition" }
  int a = [] (int u = undef ()) {
    return u;
  }();
} s2; // { dg-error "call to consteval function" }
