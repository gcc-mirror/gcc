// { dg-options "-std=c++98 -Wc++0x-compat" }

// Test warning for use of auto in C++98 mode with C++0x
// compatibility warnings
void f()
{
  auto int x = 5; // { dg-warning "will change meaning" }
}
