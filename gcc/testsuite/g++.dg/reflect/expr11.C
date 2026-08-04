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
      auto r = ^^int; // { dg-error "consteval-only value" }
      constexpr auto cr = ^^int;
    }
  if constexpr (auto r = ^^int;  // { dg-error "consteval-only value" }
		r == ^^int);	 // { dg-error "the value of .r. is not usable" }
  if constexpr (constexpr auto r = ^^int; r == ^^int);
  if constexpr (q != ^^char);
  if constexpr (^^int != ^^char);
  if (q != ^^char);  // { dg-error "consteval-only value" }
  if (^^char == ^^char);  // { dg-error "consteval-only value" }
  while (^^char == ^^char);  // { dg-error "consteval-only value" }
  do {} while (^^char == ^^char);  // { dg-error "consteval-only value" }
  consteval {
    if (q != ^^char);
    if (^^char == ^^char);
    while (^^char != ^^char);
    do {} while (^^char != ^^char);
  }

  if constexpr (true)
    {
      auto r = ^^int; // { dg-error "consteval-only value" }
    }
  else
    {
      auto r = ^^int; // { dg-error "consteval-only value" }
    }
  if constexpr (false)
    {
      auto r = ^^int; // { dg-error "consteval-only value" }
    }
  else
    {
      auto r = ^^int; // { dg-error "consteval-only value" }
    }
}
