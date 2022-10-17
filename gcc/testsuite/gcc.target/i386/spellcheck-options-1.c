/* Verify that we provide a hint if the user misspells an option argument
   (PR middle-end/77475).  */

/* { dg-do compile } */
/* { dg-options "-march=hasvel" } */
/* { dg-error "bad value 'hasvel' for '-march=' switch"  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments to '-march=' switch are: \[^\n\r]*; did you mean 'haswell'?"  "" { target *-*-* } 0 } */
