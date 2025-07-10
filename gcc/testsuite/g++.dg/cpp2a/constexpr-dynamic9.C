// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }

#if __cpp_constexpr_exceptions >= 202411L
namespace std {
  struct exception {
    constexpr exception () noexcept {}
    constexpr virtual ~exception () noexcept {}
    constexpr exception (const exception &) = default;
    constexpr exception &operator= (const exception &) = default;
    constexpr virtual const char *what () const noexcept { return "std::exception"; }
  };
  struct bad_cast : public exception {
    constexpr virtual ~bad_cast () noexcept {}
    constexpr virtual const char *what () const noexcept { return "std::bad_cast"; }
  };
}
#endif

// Ambiguous base.

struct A { virtual void a(); };
struct B : A { virtual void b(); };
struct C : A { virtual void c(); };
struct D { virtual void a(); };
struct E : B, C, D { virtual void d(); };

constexpr E e;

constexpr bool b1 = (dynamic_cast<A&>((D&)e), false); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message ".A. is an ambiguous base class of dynamic type .E. of its operand" "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }

static_assert (dynamic_cast<A*>((D*)&e) == nullptr);
