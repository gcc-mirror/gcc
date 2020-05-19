// PR c++/70001
// { dg-do compile { target c++11 } }
// { dg-require-effective-target size24plus }

struct B
{
  int a;
  constexpr B () : a (0) { }
};

struct A
{
  B b[1 << 19];
} c;
