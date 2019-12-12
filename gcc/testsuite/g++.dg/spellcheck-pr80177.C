// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

void pr80177 ()
{
  static_assertion (1 == 0, "1 == 0"); // { dg-error "3: 'static_assertion' was not declared in this scope; did you mean 'static_assert'\\?" }
  /* { dg-begin-multiline-output "" }
   static_assertion (1 == 0, "1 == 0");
   ^~~~~~~~~~~~~~~~
   static_assert
     { dg-end-multiline-output "" } */
}
