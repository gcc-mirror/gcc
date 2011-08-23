// PR c++/46862
// { dg-do compile }

namespace std
{
  namespace decimal
  {
    class decimal32 { };	// { dg-error "does not have any fields" }
    class decimal64 { };	// { dg-error "does not have any fields" }
    class decimal128 { };	// { dg-error "does not have any fields" }
  }
}

void
foo (std::decimal::decimal32 x,
     std::decimal::decimal64 y,
     std::decimal::decimal128 z)
{
}
