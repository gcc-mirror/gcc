// PR c++/124977
// { dg-do compile { target c++26 } }
// { dg-options "-Wall" }

struct Empty {};

void
foo (auto f, auto value)
{
  auto [...args] = value; // { dg-bogus "structured binding declaration set but not used" }
  (..., f (args));
}

template void foo (int, Empty);
