// PR c++/48935
// { dg-options -std=c++0x }

enum class ENUM { a };

ENUM::Type func() { return ENUM::a; } // { dg-error "does not name a type" }
