// PR105726
// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -Warray-bounds" }

#include <array>
#include <cstring>

struct X {
    char pad[4];
    std::array<char, 1> mField;
};

void encode(char* aBuffer, const X& aMessage) {
    strncpy(aBuffer, aMessage.mField.data(), 1); // { dg-bogus "bounds" }
}
