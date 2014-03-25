// PR c++/60375
// { dg-do compile { target c++11 } }

struct A
{
  decltype( [](){ return this; }() ) x; // { dg-error "unevaluated" }
};
