// { dg-do compile { target c++11 } }

#include <cstddef>

bool operator"" _yn(const char*, size_t);

typedef bool (*pfunk)(const char*, size_t);
pfunk p = &operator"" _yn;

bool tf = p("Hello,\0 World!", 14);
