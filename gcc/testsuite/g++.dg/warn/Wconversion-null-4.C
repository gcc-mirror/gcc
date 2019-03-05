// { dg-do compile  }
// { dg-options "-Wconversion-null -fdiagnostics-show-caret" }

#include <stddef.h>

void callee_1 (int, int, int) {} // { dg-message "declared here" }

void caller_1 (void)
{
  callee_1 (0, NULL, 2); // { dg-warning "passing NULL to non-pointer argument 2 of" }
  /* { dg-begin-multiline-output "" }
   callee_1 (0, NULL, 2);
                ^~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 void callee_1 (int, int, int) {}
                     ^~~
     { dg-end-multiline-output "" } */
}

void callee_2 (int, void *, int) {} // { dg-message "declared here" "" { target { ! c++11 } } }
// { dg-message "initializing argument 2 of " "" { target c++11 } .-1 }

void caller_2 (void)
{
  callee_2 (0, false, 2); // { dg-warning "converting 'false' to pointer type for argument 2 of " "" { target { ! c++11 } } }
  // { dg-error "cannot convert" "" { target c++11 } .-1 }

  /* { dg-begin-multiline-output "" }
   callee_2 (0, false, 2);
                ^~~~~
     { dg-end-multiline-output "" { target { ! c++11 } } } */
  /* { dg-begin-multiline-output "" }
   callee_2 (0, false, 2);
                ^~~~~
                |
                bool
     { dg-end-multiline-output "" { target c++11 } } */
  /* { dg-begin-multiline-output "" }
 void callee_2 (int, void *, int) {}
                     ^~~~~~
     { dg-end-multiline-output "" } */
}
