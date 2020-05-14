// { dg-do compile { target c++20 } }

// Tests constrained decltype(auto).

template<typename T>
concept Type = true;

template<typename T>
concept Int = __is_same_as(T, int);

template<typename T, typename U>
concept SameAs = __is_same_as(T, U);

template<typename T, typename U>
  requires SameAs<T, U>
constexpr bool check = true;

int z = 0;
const int cz = 0;

Type decltype(auto) x1 = 0;
static_assert(check<decltype(x1), int>);
Type decltype(auto) x2 = z;
static_assert(check<decltype(x2), int>);
Type decltype(auto) x3 = (z);
static_assert(check<decltype(x3), int&>);
Type decltype(auto) x4 = cz;
static_assert(check<decltype(x4), const int>);
Type decltype(auto) x5 = (cz);
static_assert(check<decltype(x5), const int&>);

Type decltype(auto) f1() { return 0; }
static_assert(check<decltype(f1()), int>);
Type decltype(auto) f2() { return z; }
static_assert(check<decltype(f2()), int>);
Type decltype(auto) f3() { return (z); }
static_assert(check<decltype(f3()), int&>);
Type decltype(auto) f4() { return cz; }
static_assert(check<decltype(f4()), int>); // Top-level const is removed?
Type decltype(auto) f5() { return (cz); }
static_assert(check<decltype(f5()), const int&>);

bool b = true;
const bool cb = true;

Int decltype(auto) b1 = true; // { dg-error "deduced initializer" }
Int decltype(auto) b2 = (b);  // { dg-error "deduced initializer" }
Int decltype(auto) b3 = (cb); // { dg-error "deduced initializer" }

Int decltype(auto) g1() { } // { dg-error "deduced return type" }
Int decltype(auto) g2() { return; } // { dg-error "deduced return type" }
Int decltype(auto) g3() { return true; } // { dg-error "deduced return type" }
int g4(Type decltype(auto) x) { return 0; } // { dg-error "cannot declare" }
int g5(decltype(auto) x) { return 0; } // { dg-error "cannot declare" }

template<Type decltype(auto) X, typename T>
  requires SameAs<decltype(X), T>
constexpr bool deduced_as = true;

constexpr int Z = 10;

static_assert(deduced_as<0, int>);
static_assert(deduced_as<0, int&>); // { dg-error "invalid variable template" }
static_assert(deduced_as<Z, const int>);
static_assert(deduced_as<(Z), const int>); // { dg-error "invalid variable template" }
static_assert(deduced_as<(Z), const int&>);

