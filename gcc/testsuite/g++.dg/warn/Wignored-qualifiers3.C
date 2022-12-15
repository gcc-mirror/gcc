// PR c++/107492
// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wignored-qualifiers" }

// Here the 'const' matters, so don't warn.
template<typename T> struct S { };
template<> struct S<void(*)()> { };
template<> struct S<const void(*)()> { }; // { dg-bogus "ignored" }

template<typename T, typename U> constexpr bool is_same_v = false;
template<typename T> constexpr bool is_same_v<T, T> = true;

static_assert( ! is_same_v< void(*)(), const void(*)() >, ""); // { dg-bogus "ignored" }

// Here the 'const' matters as well -> don't warn.
auto g() -> const void (*)(); // { dg-bogus "ignored" }
auto g() -> const void (*)() { return nullptr; } // { dg-bogus "ignored" }

// Here as well.
const void (*h)() = static_cast<const void (*)()>(h); // { dg-bogus "ignored" }

// But let's keep the warning here.
const void f(); // { dg-warning "ignored" }
const void f() { } // { dg-warning "ignored" }
