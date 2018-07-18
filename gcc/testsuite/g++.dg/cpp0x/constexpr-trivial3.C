// PR c++/80077
// { dg-do compile { target c++11 } }
// { dg-options -fno-elide-constructors }

struct X_t { X_t() = default; };
constexpr X_t x = X_t();
