// PR c++/96380
// { dg-do compile { target c++11 } }

extern const int a, b;
enum struct c;
template <class>
enum struct c : union enum struct c { e = b, f = a }; // { dg-error "types may not be defined|expected|elaborated-type-specifier" }
