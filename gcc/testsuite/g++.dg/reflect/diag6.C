// PR c++/124489
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

void
g ()
{
  constexpr decltype(^^::) dm = ^^int; // { dg-message ".constexpr std::meta::info dm. previously declared here" }
  constexpr decltype(^^::) dm = ^^int; // { dg-error "redeclaration of .constexpr std::meta::info dm." }
}
