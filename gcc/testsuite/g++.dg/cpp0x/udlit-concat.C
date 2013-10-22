// { dg-options "-std=c++11" }

#include <string>

std::string operator"" _www(const char*, size_t);

std::string concat01 = "Hello, " "World!"_www;

std::string concat10 = "Hello, "_www "World!";

std::string concat11 = "Hello, "_www "World!"_www;


class Tachyon { };

Tachyon operator"" _fast(const char*, size_t);

int operator"" _fast(const char32_t*, size_t);

int speedy01 = "Hello, " U"World!"_fast;

int speedy10 = "Hello, "_fast U"World!";

int speedy11 = "Hello, "_fast U"World!"_fast;
