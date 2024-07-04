// P2564R3
// { dg-do compile { target c++20 } }
// The problem here was that while parsing, we first process calling
// 'f' from 'g' but only when instantiating 'f<int>' do we promote 'f'
// to consteval.  When the var we're initializing is marked constexpr,
// store_init_value detects the problem that we're calling a consteval
// function with non-const argument.

consteval int id(int i) { return i; }

// Don't let the instantiations confuse us, e.g. instantiating a fn
// prior to entering 'g'.
template <typename T>
constexpr int f1(T t) { return id (t); }

template <typename T>
constexpr int f2(T t) { return id (t); }

template <typename T>
constexpr int f3(T t) { return id (t); }

template <typename T>
constexpr int f4(T t) { return id (t); }

template <typename T>
constexpr int f5(T t) { return id (t); }

template <typename T>
constexpr int f6(T t) { return id (t); }

template <typename T>
constexpr int f7(T t) { return id (t); }

template <typename T>
constexpr int f8(T t) { return id (t); }

template <typename T>
constexpr int f9(T t) { return id (t); }

template <typename T>
constexpr int f10(T t) { return id (t); }

template <typename T>
constexpr int g1(T t) { auto p = id; return p (t); }

int non_const;

auto a1 = f1 (non_const); // { dg-error "call to consteval function|not usable" }
constexpr auto a2 = f2 (non_const); // { dg-error "not a constant|not usable" }
auto a3 = f3 (42);
constexpr auto a4 = f4 (42);

void
g ()
{
   auto a5 = f5 (non_const); // { dg-error "not a constant|not usable" }
   constexpr auto a6 = f6 (non_const); // { dg-error "not usable" }
   auto a7 = f7 (42);
   constexpr auto a8 = f8 (42);
   (void) f9 (non_const); // { dg-error "not a constant|not usable" }
   (void) f10 (42);
   (void) g1 (non_const); // { dg-error "not a constant|not usable" }
}

struct S {
    int y;
    int x = id (y);
    // Promoted to consteval.
    template<typename T>
    constexpr S(T t) : y (id (t)) {}
};

S s1(1);
S s2(non_const); // { dg-error "call to consteval function|not usable" }
constexpr S s3(1);
constexpr S s4(non_const); // { dg-error "not usable" }
