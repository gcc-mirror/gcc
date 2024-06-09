// PR c++/114706
// { dg-do compile { target c++20 } }

namespace std {
template<typename T, typename F>
constexpr T
bit_cast (const F& f) noexcept
{
  return __builtin_bit_cast (T, f);
}
}
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'const A' contains a union type" "" { target *-*-* } .-3 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'A' contains a union type" "" { target *-*-* } .-4 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'const C' contains a union type" "" { target *-*-* } .-5 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'C' contains a union type" "" { target *-*-* } .-6 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'const E' contains a pointer type" "" { target *-*-* } .-7 }
// { dg-error "'__builtin_bit_cast' is not a constant expression because 'E' contains a pointer type" "" { target *-*-* } .-8 }

union U { int a; int b; };
struct A { U c[2]; };
struct B { int d[2]; };
struct C { U e[2][2]; };
struct D { int f[2][2]; };
struct E { int *g[3]; };
struct F { char h[sizeof (int *) * 3]; };
constexpr B i = std::bit_cast<B> (A{});	// { dg-message "in 'constexpr' expansion of 'std::bit_cast<" }
constexpr A j = std::bit_cast<A> (B{});	// { dg-message "in 'constexpr' expansion of 'std::bit_cast<" }
constexpr D k = std::bit_cast<D> (C{});	// { dg-message "in 'constexpr' expansion of 'std::bit_cast<" }
constexpr C l = std::bit_cast<C> (D{});	// { dg-message "in 'constexpr' expansion of 'std::bit_cast<" }
constexpr F m = std::bit_cast<F> (E{});	// { dg-message "in 'constexpr' expansion of 'std::bit_cast<" }
constexpr E n = std::bit_cast<E> (F{});	// { dg-message "in 'constexpr' expansion of 'std::bit_cast<" }
