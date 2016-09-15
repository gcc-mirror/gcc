/* Verify that we provide a hint if the user misspells an option argument
   (PR middle-end/77475).  */

/* { dg-do compile } */

__attribute__((target ("arch=hasvel"))) void foo (void) {} /* { dg-error "bad value .'hasvel'. for 'target..arch=..' attribute" } */
/* { dg-message "valid arguments to 'target..arch=..' attribute are: \[^\n\r]*; did you mean 'haswell'?"  "" { target *-*-* } 6 } */
