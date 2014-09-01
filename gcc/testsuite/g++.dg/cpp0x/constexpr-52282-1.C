// PR c++/52282
// { dg-do compile { target c++11 } }

template <typename T, T V>
struct A
    {
    static constexpr T a() { return V; }
    };

template <typename T, T V>
struct B
    {
    typedef T type;
    static constexpr type b() { return V; }
    };

template <typename T, T V>
struct C
    {
    static constexpr decltype(V) c() { return V; }
    };
static_assert(A<int, 10>::a() == 10, "oops");
static_assert(B<int, 10>::b() == 10, "oops");
static_assert(C<int, 10>::c() == 10, "oops");

struct D
    {
    static constexpr int d() { return 10; }
    };
static_assert((A<int(*)(), &D::d>::a())() == 10, "oops");
static_assert((B<int(*)(), &D::d>::b())() == 10, "oops");
static_assert((C<int(*)(), &D::d>::c())() == 10, "oops");
