// { dg-options -std=c++11 }

#include <string>

std::string operator"" 5X(const char*, std::size_t);	// { dg-error "expected suffix identifier" }
