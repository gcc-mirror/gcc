// PR c++/88815
// { dg-do compile { target c++11 } }

struct true_type {
    constexpr operator bool() const { return true; }
};

struct false_type {
    constexpr operator bool() const { return false; }
};

template<int (*p)()>
true_type is_constexpr_impl(decltype(int{(p(), 0U)}));

template<int (*p)()>
false_type is_constexpr_impl(...);

template<int (*p)()>
using is_constexpr = decltype(is_constexpr_impl<p>(0));

constexpr int f() { return 0; }
int g() { return 0; }

static_assert(is_constexpr<f>(), "");
static_assert(!is_constexpr<g>(), "");
