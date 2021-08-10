// P0466R5
// { dg-do compile { target c++20 } }

namespace std
{
template <class S, class M>
constexpr bool
is_pointer_interconvertible_with_class (M S::*m) noexcept
{
  return __builtin_is_pointer_interconvertible_with_class (m);
}
}

struct A;
struct B { int b; double b2; };
struct C : virtual B { int c; };
struct D {};
struct E {};
struct F : public B, D, E {};
struct G : public D, E { int g; };
struct H {};
struct I : public G, H {};
struct J { int j1; private: int j2; public: int j3; };
struct K : public J {};
struct L : public B, D, E {};
struct M { D d [[no_unique_address]]; E e [[no_unique_address]]; int f; };
union U { int a; double b; long long c; };
struct V { union { int a; long b; }; int c; };
union X { int a; union { short b; long c; }; long long d; };
struct Y { void foo () {} };
union Z { int a; private: int b; protected: int c; public: int d; };

static_assert (std::is_pointer_interconvertible_with_class (&B::b));
static_assert (!std::is_pointer_interconvertible_with_class (&B::b2));
static_assert (std::is_pointer_interconvertible_with_class (&C::b));
static_assert (std::is_pointer_interconvertible_with_class (&F::b));
static_assert (std::is_pointer_interconvertible_with_class<F, int> (&F::b));
static_assert (std::is_pointer_interconvertible_with_class (&G::g));
static_assert (std::is_pointer_interconvertible_with_class<G, int> (&G::g));
static_assert (std::is_pointer_interconvertible_with_class (&I::g));
static_assert (std::is_pointer_interconvertible_with_class<I, int> (&I::g));
static_assert (!std::is_pointer_interconvertible_with_class (&J::j1));
static_assert (!std::is_pointer_interconvertible_with_class (&J::j3));
static_assert (!std::is_pointer_interconvertible_with_class (&K::j1));
static_assert (!std::is_pointer_interconvertible_with_class (&K::j3));
static_assert (std::is_pointer_interconvertible_with_class (&L::b));
static_assert (std::is_pointer_interconvertible_with_class<L, int> (&L::b));
static_assert (std::is_pointer_interconvertible_with_class (&L::b));
static_assert (std::is_pointer_interconvertible_with_class (&M::d));
static_assert (!std::is_pointer_interconvertible_with_class (&M::e));
static_assert (!std::is_pointer_interconvertible_with_class (&M::f));
static_assert (std::is_pointer_interconvertible_with_class (&U::a));
static_assert (std::is_pointer_interconvertible_with_class (&U::b));
static_assert (std::is_pointer_interconvertible_with_class (&U::c));
static_assert (std::is_pointer_interconvertible_with_class (&V::a));
static_assert (std::is_pointer_interconvertible_with_class (&V::b));
static_assert (!std::is_pointer_interconvertible_with_class (&V::c));
static_assert (std::is_pointer_interconvertible_with_class (&X::a));
static_assert (std::is_pointer_interconvertible_with_class (&X::b));
static_assert (std::is_pointer_interconvertible_with_class (&X::c));
static_assert (std::is_pointer_interconvertible_with_class (&X::d));
static_assert (!std::is_pointer_interconvertible_with_class ((int B::*) nullptr));
static_assert (!std::is_pointer_interconvertible_with_class (&Y::foo));
static_assert (std::is_pointer_interconvertible_with_class (&Z::a));
static_assert (std::is_pointer_interconvertible_with_class (&Z::d));
