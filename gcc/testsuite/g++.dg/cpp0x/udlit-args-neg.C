// { dg-options -std=c++0x }

#include <cstddef>

class Foo { };

int
operator"" _Foo();	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(int *);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(unsigned long int);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(double);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(const float *, std::size_t);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(const wchar_t *, int);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(const char16_t *);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(char...);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(unsigned long long int, char);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(const char *, std::size_t, int);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(long double &);	// { dg-error "has invalid argument list" }

Foo
operator"" _Foo(std::size_t, const char16_t *);	// { dg-error "has invalid argument list" }
