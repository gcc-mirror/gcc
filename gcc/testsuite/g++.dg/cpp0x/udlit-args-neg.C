// { dg-do compile { target c++11 } }

#include <cstddef>

class Foo { };

int
operator ""_Foo();	// { dg-error "1:.int operator\"\"_Foo\\(\\). has invalid argument list" }

Foo
operator ""_Foo(int *);	// { dg-error "1:.Foo operator\"\"_Foo\\(int\\*\\). has invalid argument list" }

Foo
operator ""_Foo(unsigned long int);	// { dg-error "1:.Foo operator\"\"_Foo\\(long unsigned int\\). has invalid argument list" }

Foo
operator ""_Foo(double);	// { dg-error "1:.Foo operator\"\"_Foo\\(double\\). has invalid argument list" }

Foo
operator ""_Foo(const float *, std::size_t);	// { dg-error "1:.Foo operator\"\"_Foo\\(const float\\*, std::size_t\\). has invalid argument list" }

Foo
operator ""_Foo(const wchar_t *, int);	// { dg-error "1:.Foo operator\"\"_Foo\\(const wchar_t\\*, int\\). has invalid argument list" }

Foo
operator ""_Foo(const char16_t *);	// { dg-error "1:.Foo operator\"\"_Foo\\(const char16_t\\*\\). has invalid argument list" }

Foo
operator ""_Foo(char...);	// { dg-error "1:.Foo operator\"\"_Foo\\(char, \\.\\.\\.\\). has invalid argument list" }

Foo
operator ""_Foo(unsigned long long int, char);	// { dg-error "1:.Foo operator\"\"_Foo\\(long long unsigned int, char\\). has invalid argument list" }

Foo
operator ""_Foo(const char *, std::size_t, int);	// { dg-error "1:.Foo operator\"\"_Foo\\(const char\\*, std::size_t, int\\). has invalid argument list" }

Foo
operator ""_Foo(long double &);	// { dg-error "1:.Foo operator\"\"_Foo\\(long double&\\). has invalid argument list" }

Foo
operator ""_Foo(std::size_t, const char16_t *);	// { dg-error "1:.Foo operator\"\"_Foo\\(std::size_t, const char16_t\\*\\). has invalid argument list" }
