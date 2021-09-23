int 1;
/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret" } */

/* This file begins with a UTF-8 byte order mark.  Verify that diagnostics
   still point to the right place, since the stripping of the BOM happens twice,
   once when libcpp reads the file, and once when diagnostics infrastucture
   reads it.  */

/* { dg-error "expected .* before numeric constant" "" { target *-*-*} 1 } */
/* { dg-begin-multiline-output "" }
 int 1;
     ^
   { dg-end-multiline-output "" } */
