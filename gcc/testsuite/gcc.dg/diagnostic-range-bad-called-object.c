/* { dg-options "-fdiagnostics-show-caret" } */

/* Adapted from https://gcc.gnu.org/wiki/ClangDiagnosticsComparison */

void call_of_non_function_ptr (char **argP, char **argQ)
{
  (argP - argQ)(); /* { dg-error "called object is not a function or function pointer" } */

/* { dg-begin-multiline-output "" }
   (argP - argQ)();
   ~~~~~~^~~~~~~
   { dg-end-multiline-output "" } */

  argP();       /* { dg-error "called object 'argP' is not a function or function pointer" } */

/* { dg-begin-multiline-output "" }
   argP();
   ^~~~
   { dg-end-multiline-output "" }
   { dg-begin-multiline-output "" }
 void call_of_non_function_ptr (char **argP, char **argQ)
                                ~~~~~~~^~~~
   { dg-end-multiline-output "" } */
}
