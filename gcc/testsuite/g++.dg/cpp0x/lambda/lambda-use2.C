// PR c++/50224
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-parameter" }

struct T;

void m(T& t) // ERROR here
{
  [&]{
    t; // ``t`` is referenced here
  };
}
