// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// ADL.  The namespace std::meta is an associated namespace of
// std::meta::info, which allows standard library meta functions
// to be invoked without explicit qualification.

#include <meta>

struct S {};

bool b1 = std::meta::has_identifier (^^S);
bool b2 = has_identifier (^^S);


std::string_view name2 = std::meta::identifier_of(^^S);  // Okay.
std::string_view name1 = identifier_of(^^S);             // Also okay.
