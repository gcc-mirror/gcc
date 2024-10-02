// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

std::string operator ""5X(const char*, std::size_t);	// { dg-error "expected suffix identifier" }
