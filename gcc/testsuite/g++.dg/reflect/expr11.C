// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections in if and while.

using info = decltype(^^void);
consteval info foo (info i) { return i; }

void
f ()
{
  constexpr auto q = ^^float;
  if constexpr (foo (^^::) == ^^::)
    {
      auto r = ^^int; // { dg-error "consteval-only variable .r." }
      constexpr auto cr = ^^int;
    }
  if constexpr (auto r = ^^int;  // { dg-error "consteval-only variable .r." }
		r == ^^int);	 // { dg-error "the value of .r. is not usable" }
  if constexpr (constexpr auto r = ^^int; r == ^^int);
  if constexpr (q != ^^char);
  if constexpr (^^int != ^^char);
  if (q != ^^char);  // { dg-error "consteval-only expressions" }
  if (^^char == ^^char);  // { dg-error "consteval-only expressions" }
  while (^^char == ^^char);  // { dg-error "consteval-only expressions" }
  do {} while (^^char == ^^char);  // { dg-error "consteval-only expressions" }
  consteval {
    if (q != ^^char);
    if (^^char == ^^char);
    while (^^char != ^^char);
    do {} while (^^char != ^^char);
  }
}
