// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }
// { dg-prune-output "concept definition syntax is" }

template<typename T>
  concept auto C1() { return 0; } // { dg-error "16:function concepts are no longer supported" }

template<typename T>
  concept int C2() { return 0; } // { dg-error "15:function concepts are no longer supported" }
