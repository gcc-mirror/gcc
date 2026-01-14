// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from the P2996 paper.

#include <meta>

static_assert(std::meta::is_type(^^int()));  // ^^ applies to the type-id "int()"

template<bool> struct X {};
consteval bool operator<(std::meta::info, X<false>) { return false; }
consteval void g(std::meta::info r, X<false> xv) {
  r == ^^int && true;    // { dg-error "expected" }
  r == ^^int & true;     // { dg-error "expected" }
  r == (^^int) && true;  // OK
  r == ^^int &&&& true;  // { dg-error "expected|cannot declare" }
  ^^X < xv;              // { dg-error "could not convert" }
			 // error: reflect-expression whose terminal name is a
                         // template-name is followed by <
  (^^X) < xv;            // OK
}
