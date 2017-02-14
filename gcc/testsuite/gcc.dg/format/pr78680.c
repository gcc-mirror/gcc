/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wextra -fdiagnostics-show-caret" } */

void fn1() {
  __builtin_printf("\
     %ld.\n\
        2\n"); };
/* { dg-warning "expects a matching" "" { target *-*-* } .-3 } */
/* { dg-begin-multiline-output "" }
   __builtin_printf("\
                    ^~
      %ld.\n\
      ~~~~~~~        
         2\n"); };
         ~~~~        
   { dg-end-multiline-output "" } */
