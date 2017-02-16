// PR c++/70001

// This is still slow to compile, only run it once.
// { dg-do compile { target c++14_only } }

struct B
{
  int a;
  constexpr B () : a (0) { }
};

struct A
{
  B b[1 << 19][1][1][1];
} c;
