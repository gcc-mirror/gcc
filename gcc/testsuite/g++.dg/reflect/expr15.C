// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

void
f1 ()
{
  int x = 1; // { dg-message ".x. declared here" }
  constexpr auto r = ^^x;
  [=] -> decltype(x) {
    return [:r:]; // { dg-error "use of local variable" }
  };
}

void
f2 ()
{
  int x = 1; // { dg-message ".x. declared here" }
  constexpr auto r = ^^x;
  [&] -> decltype(x) {
    return [:r:]; // { dg-error "use of local variable" }
  };
}
