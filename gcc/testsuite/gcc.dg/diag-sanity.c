/* Verify that an invalid argument is diagnosed correcly.
   { dg-do compile }
   { dg-options "-fdiagnostics-minimum-margin-width=42xyz -flto-compression-level=2-O2" } */


/* { dg-error "argument to '-fdiagnostics-minimum-margin-width=' should be a non-negative integer" "" { target *-*-* } 0 }
   { dg-error "argument to '-flto-compression-level=' should be a non-negative integer" "" { target *-*-* } 0 } */
