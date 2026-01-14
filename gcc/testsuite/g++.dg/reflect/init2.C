// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test various forms of initialization with reflections.  Invalid.

consteval void
f ()
{
  constexpr static auto srefl = ^^int;
  constexpr auto *p = &srefl;
  constexpr auto **q = &p;  // { dg-error "unable to deduce" }
  // { dg-message "types .auto\\*. and .std::meta::info\\* const." "" { target *-*-* } .-1 }
}

void
g ()
{
  f ();
}
