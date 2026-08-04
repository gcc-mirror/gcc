// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we suggest adding "constexpr" (where allowed).

auto foo = ^^int;  // { dg-error ".foo. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
constinit auto foo_ = ^^int; // { dg-error ".foo_. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
constexpr auto foo__ = ^^int;
thread_local auto tfoo = ^^int;  // { dg-error ".tfoo. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
thread_local constinit auto tfoo_ = ^^int; // { dg-error ".tfoo_. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
thread_local constexpr auto tfoo__ = ^^int;

void
f ()
{
  auto ref = ^^int;  // { dg-error ".ref. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
  constexpr auto ref_ = ^^int;
  static auto sref = ^^int;  // { dg-error ".sref. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
  static auto constinit sref_ = ^^int; // { dg-error ".sref_. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
  static auto constexpr sref__ = ^^int;
  thread_local auto tref = ^^int; // { dg-error ".tref. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
  thread_local constinit auto tref_ = ^^int; // { dg-error ".tref_. is initialized with a consteval-only value" }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
  thread_local constexpr auto tref__ = ^^int;
}
