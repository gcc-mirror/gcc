// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we suggest adding "constexpr" or "constinit" (where allowed).

auto foo = ^^int;  // { dg-error "consteval-only variable .foo." }
// { dg-message "add .constexpr. or .constinit." "" { target *-*-* } .-1 }
constinit auto foo_ = ^^int;
constexpr auto foo__ = ^^int;
thread_local auto tfoo = ^^int;  // { dg-error "consteval-only variable .tfoo." }
// { dg-message "add .constexpr. or .constinit." "" { target *-*-* } .-1 }
thread_local constinit auto tfoo_ = ^^int;
thread_local constexpr auto tfoo__ = ^^int;

void
f ()
{
  auto ref = ^^int;  // { dg-error "consteval-only variable .ref." }
// { dg-message "add .constexpr." "" { target *-*-* } .-1 }
  constexpr auto ref_ = ^^int;
  static auto sref = ^^int;  // { dg-error "consteval-only variable .sref." }
// { dg-message "add .constexpr. or .constinit." "" { target *-*-* } .-1 }
  static auto constinit sref_ = ^^int;
  static auto constexpr sref__ = ^^int;
  thread_local auto tref = ^^int; // { dg-error "consteval-only variable .tref." }
// { dg-message "add .constexpr. or .constinit." "" { target *-*-* } .-1 }
  thread_local constinit auto tref_ = ^^int;
  thread_local constexpr auto tref__ = ^^int;
}
