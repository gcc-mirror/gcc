// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept auto C1() { return 0; } // { dg-error "deduced return type" }

template<typename T>
  concept int C2() { return 0; } // { dg-error "return type" }
