// PR c++/60375
// { dg-do compile { target c++11 } }

struct A
{
  decltype( [](){ return this; }() ) x; // { dg-error "unevaluated" "" { target c++17_down } }
  // { dg-error "not captured" "" { target c++2a } .-1 }
};

// { dg-prune-output "declared void" }
