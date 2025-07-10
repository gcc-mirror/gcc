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

// Unrelated type.

struct B { virtual void b(); };
struct P1 { virtual void p1(); };
struct P2 { virtual void p2(); };
struct A : public B, private P1, protected P2 { virtual void a(); };

constexpr A a;

struct U { virtual void u(); };

constexpr bool b1 = (dynamic_cast<U&>((B&)a), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "dynamic type .A. of its operand does not have an unambiguous public base class .U." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }
constexpr bool b2 = (dynamic_cast<U&>((P1&)a), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "static type .const P1. of its operand is a non-public base class of dynamic type .A." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }
constexpr bool b3 = (dynamic_cast<U&>((P2&)a), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "static type .const P2. of its operand is a non-public base class of dynamic type .A." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }

static_assert (dynamic_cast<U*>((B*)&a) == nullptr);
static_assert (dynamic_cast<U*>((P1*)&a) == nullptr);
static_assert (dynamic_cast<U*>((P2*)&a) == nullptr);
