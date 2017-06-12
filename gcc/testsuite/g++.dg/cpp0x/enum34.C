// { dg-options "-fdiagnostics-show-caret" }
// { dg-do compile { target c++11 } }

enum class E;

enum class E e;  /* { dg-warning "scoped enum must not use" }
  { dg-begin-multiline-output "" }
 enum class E e;
 ~~~~ ^~~~~
      -----
  { dg-end-multiline-output "" } */
