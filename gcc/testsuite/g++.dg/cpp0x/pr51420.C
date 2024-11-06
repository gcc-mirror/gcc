// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

void
foo()
{
  float x = operator ""_F();  //  { dg-error  "13:'operator\"\"_F' was not declared in this scope" }
  /* { dg-begin-multiline-output "" }
   float x = operator ""_F();
             ^~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  float y = 0_F;  //  { dg-error  "unable to find numeric literal operator" }
  /* { dg-begin-multiline-output "" }
   float y = 0_F;
             ^~~
     { dg-end-multiline-output "" } */
}
