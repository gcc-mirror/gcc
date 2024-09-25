// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-additional-options "-Wall" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
// -O1 doesn't iterate VN and thus has bogus uninit diagnostics
// { dg-skip-if "" { *-*-* } { "-O1" } { "" } }

// The testcase still emits bogus diagnostics with the pre-C++11 ABI
#undef _GLIBCXX_USE_CXX11_ABI
#define _GLIBCXX_USE_CXX11_ABI 1

#include <vector>

// When the library is not dual-ABI and defaults to old just compile
// an empty TU
#if _GLIBCXX_USE_CXX11_ABI

#include <optional>
template <class T>
using Optional = std::optional<T>;

#include <sstream>

struct MyOptionalStructWithInt {
    int myint; /* works without this */
    Optional<std::vector<std::string>> myoptional;
};

struct MyOptionalsStruct {
    MyOptionalStructWithInt external1;
    MyOptionalStructWithInt external2;
};

struct MyStruct { };
std::ostream &operator << (std::ostream &os, const MyStruct &myStruct);

std::vector<MyStruct> getMyStructs();

void test()
{
    MyOptionalsStruct externals;
    MyOptionalStructWithInt internal1;
    MyOptionalStructWithInt internal2;

    std::vector<MyStruct> myStructs;
    myStructs = getMyStructs();

    for (const auto& myStruct : myStructs)
    {
        std::stringstream address_stream;
        address_stream << myStruct;
        internal1.myint = internal2.myint = 0;
        externals.external1 = internal1;
        externals.external2 = internal2;
        externals.external2 = internal2;
    }
}
#endif
