// PR c++/48935
// { dg-do compile { target c++11 } }

enum class ENUM { a };

ENUM::Type func() { return ENUM::a; } // { dg-error "does not name a type" }
