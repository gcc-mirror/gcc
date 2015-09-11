// { dg-do compile { target c++11 } }

struct L { constexpr operator int() const { return 0; } };
constexpr L LVar{};

template<const L&> int *f() { return 0; }
template<int> char *f();

auto r = f<LVar>();		// { dg-error "ambiguous" }
