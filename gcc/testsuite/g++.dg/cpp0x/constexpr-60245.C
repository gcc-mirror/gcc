// PR c++/60245
// { dg-do compile { target c++11 } }

constexpr int Apply(const int in, int (*f)(const int&)) { return f(in); }

using Foo = int;
static constexpr int id(const Foo& i) { return i; }
static constexpr int results1 = Apply(0, &id);
