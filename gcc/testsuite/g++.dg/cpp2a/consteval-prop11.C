// P2564R3
// { dg-do compile { target c++20 } }
// { dg-options "-fdiagnostics-show-caret" }
// Test diagnostic.

consteval int id (int i) { return i; }
constexpr int foo (int i ) { return i; }

constexpr int
foobar (auto i)
{
  return i + id (i);
  /* { dg-begin-multiline-output "" }
   return i + id (i);
              ~~~^~~
     { dg-end-multiline-output "" } */
}

void
g (int x)
{
  foobar (x); // { dg-error "10:call to consteval function .foobar<int>\\(x\\). is not a constant expression" }
// { dg-error ".x. is not a constant expression" "" { target *-*-* } .-1 }
  /* { dg-begin-multiline-output "" }
foobar (x);
   ~~~~~~~^~~
     { dg-end-multiline-output "" } */
}

constexpr int
f2 (auto i)
{
  auto p = &id;
  /* { dg-begin-multiline-output "" }
   auto p = &id;
            ^~~
     { dg-end-multiline-output "" } */
  return p (i);
}

void
g2 (int x)
{
  f2 (x); // { dg-error "6:call to consteval function .f2<int>\\(x\\). is not a constant expression|not a constant expression" }
  /* { dg-begin-multiline-output "" }
f2 (x);
   ~~~^~~
     { dg-end-multiline-output "" } */
}
