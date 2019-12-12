// fix-it hint for missing "typename" (PR c++/63392)
// { dg-do compile { target c++17_down } }
// { dg-options "-fdiagnostics-show-caret" }

template<typename T>
class test_1 {
  T::type x; // { dg-error "need 'typename' before 'T::type' because 'T' is a dependent scope" }
  /* { dg-begin-multiline-output "" }
   T::type x;
   ^
   typename 
     { dg-end-multiline-output "" } */
};
