// PR c++/106649
// P2448 - Relaxing some constexpr restrictions
// { dg-do compile { target c++23 } }
// Test that we get a diagnostic even in C++23 if you do call the function.

constexpr unsigned int
fn0 (const int *p)
{
  return *reinterpret_cast<unsigned const int *>(p); // { dg-error ".reinterpret_cast. is not a constant expression" }
}

constexpr void *
fn1 (int i)
{
  return (void *) 1LL; // { dg-error ".reinterpret_cast." }
}

void
g ()
{
  constexpr int i = 42;
  constexpr auto a1 = fn0 (&i);
  constexpr auto a2 = fn1 (i); // { dg-error "called in a constant expression" }
}
