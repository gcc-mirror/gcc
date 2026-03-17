// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections in consteval if.

void
f ()
{
  constexpr auto q = ^^float;
  if consteval
    {
      ^^void;
      q;
      auto r = ^^int;
      if (q != ^^char);
      if (^^char == ^^char);
      while (^^char != ^^char);
      do {} while (^^char != ^^char);
    }

  if not consteval
    {
      ^^void;  // { dg-error "consteval-only expressions" }
    }
  if not consteval
    {
      q;  // { dg-error "consteval-only expressions" }
    }
  if not consteval
    {
      auto r = ^^int;  // { dg-error "consteval-only variable" }
    }
  if not consteval
    {
      if (q != ^^char);  // { dg-error "consteval-only expressions" }
    }
  if not consteval
    {
      if (^^char == ^^char);  // { dg-error "consteval-only expressions" }
    }
  if not consteval
    {
      while (^^char != ^^char);  // { dg-error "consteval-only expressions" }
    }
  if not consteval
    {
      do {} while (^^char != ^^char);  // { dg-error "consteval-only expressions" }
    }
}
