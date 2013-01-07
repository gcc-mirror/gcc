// Origin PR c++/55311
// { dg-do compile { target c++11 } }

template <const char *const C, typename T>
struct A
{};

struct B {};

extern constexpr char HELLO_WORLD[] = "hello world";

A<HELLO_WORLD, B> g; // <-- This works fine

template <typename T>
using PartiallySpecialized = A<HELLO_WORLD, T>;  // <-- This fails
