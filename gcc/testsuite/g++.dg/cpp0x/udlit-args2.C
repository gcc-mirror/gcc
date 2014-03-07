// PR c++/52521
// { dg-do compile { target c++11 } }

#include <cstddef>

int operator "" _a (const char *);
int operator "" _a (const char *, std::size_t);
int a = 123_a;
int a2 = "abc"_a;

int operator "" _b (const char *, std::size_t);
int operator "" _b (const char *);
int b = 123_b;
int b2 = "abc"_b;
