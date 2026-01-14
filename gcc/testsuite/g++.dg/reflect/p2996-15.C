// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [expr.const].

using info = decltype(^^int);

struct Base { };
struct Derived : Base { info r; };

consteval const Base& fn(const Derived& derived) { return derived; }

constexpr Derived obj{.r=^^::}; // OK
constexpr const Derived& d = obj; // OK
constexpr const Base& b1 = fn(obj); // { dg-error "reference into an object of consteval-only" }
constexpr const Base& b2 = obj;	  // { dg-error "reference into an object of consteval-only" }
constexpr Base b3 = obj;
