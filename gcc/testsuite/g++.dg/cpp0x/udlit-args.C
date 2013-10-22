// { dg-options -std=c++11 }

#include <cstddef>

class Foo { };

Foo
operator"" _Foo(const char *);

Foo
operator"" _Foo(unsigned long long int);

Foo
operator"" _Foo(long double);

Foo
operator"" _Foo(char);

Foo
operator"" _Foo(wchar_t);

Foo
operator"" _Foo(char16_t);

Foo
operator"" _Foo(char32_t);

Foo
operator"" _Foo(const char *, std::size_t);

Foo
operator"" _Foo(const wchar_t *, std::size_t);

Foo
operator"" _Foo(const char16_t *, std::size_t);

Foo
operator"" _Foo(const char32_t *, std::size_t);
