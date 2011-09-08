// PR c++/50224
// { dg-options "-std=c++0x -Wunused-parameter" }

struct T;

void m(T& t) // ERROR here
{
  [&]{
    t; // ``t`` is referenced here
  };
}
