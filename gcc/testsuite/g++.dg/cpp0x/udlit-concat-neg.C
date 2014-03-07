// { dg-do compile { target c++11 } }

#include <string>

std::string operator"" _xxx(const char*, size_t);

std::string operator"" _yyy(const char*, size_t);

std::string concat = "Hello, "_xxx "World!"_yyy;	// { dg-error "inconsistent user-defined literal suffixes" }
