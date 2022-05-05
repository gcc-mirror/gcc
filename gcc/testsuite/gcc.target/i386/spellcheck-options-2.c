/* Verify that we provide a hint if the user misspells an option argument
   (PR middle-end/77475).  */

/* { dg-do compile } */
/* { dg-options "-mtune=hasvel" } */
/* { dg-error "bad value 'hasvel' for '-mtune=' switch"  "" { target *-*-* } 0 } */
/* { dg-message "valid arguments to '-mtune=' switch are: \[^\n\r]*; did you mean 'haswell'?"  "" { target *-*-* } 0 } */
