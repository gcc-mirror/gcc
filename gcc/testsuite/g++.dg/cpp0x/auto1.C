// { dg-options "-std=c++98 -Wc++11-compat" }

// Test warning for use of auto in C++98 mode with C++11
// compatibility warnings
void f()
{
  auto int x = 5; // { dg-warning "changes meaning" }
}
