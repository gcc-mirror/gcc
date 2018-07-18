// { dg-do compile { target c++11 } }
// { dg-options "-std=c++98 -Wc++11-compat -fdiagnostics-show-caret" }

// Test warning for use of auto in C++98 mode with C++11
// compatibility warnings
void f()
{
  auto int x = 5; /* { dg-warning "changes meaning" }
  { dg-begin-multiline-output "" }
   auto int x = 5;
   ^~~~
   ----
  { dg-end-multiline-output "" } */
}
