// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
  concept auto C1() { return 0; } // { dg-error "16:concept .concept auto C1\\(\\). declared with a deduced return type" }

template<typename T>
  concept int C2() { return 0; } // { dg-error "15:concept .concept int C2\\(\\). with non-.bool. return type .int." }

template<typename T>
  concept bool C3(int) { return 0; } // { dg-error "16:concept .concept bool C3\\(int\\). declared with function parameters" }
