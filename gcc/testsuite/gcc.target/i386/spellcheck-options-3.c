/* Verify that we provide a hint if the user misspells an option argument
   (PR middle-end/77475).  */

/* { dg-do compile } */
/* { dg-options "-mmemcpy-strategy=unroled_looop:8:align" } */
/* { dg-error "wrong strategy name 'unroled_looop' specified for option '-mmemcpy_strategy='"  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments to '-mmemcpy_strategy=' are: \[^\n\r]*; did you mean 'unrolled_loop'?"  "" { target *-*-* } 0 } */
