// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test static consteval-only vars in a consteval function.

consteval void
f1 ()
{
  static auto i = ^^int;  // { dg-error ".i. defined .static. in .constexpr. context" }
}

consteval void
f2 ()
{
  static const auto i = ^^int;  // { dg-error ".i. defined .static. in .constexpr. context" }
}

consteval void
f3 ()
{
  static constexpr auto i = ^^int;
}

consteval
void
f4 ()
{
  static constinit auto i = ^^int;  // { dg-error ".i. defined .static. in .constexpr. context" }
}

void
z ()
{
  f1();  // { dg-error "call to consteval function|in a constant expression" }
  f2();  // { dg-error "call to consteval function|in a constant expression" }
  f3();
  f4();  // { dg-error "call to consteval function|in a constant expression" }
}
