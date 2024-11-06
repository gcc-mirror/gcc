/* Verify using -fdiagnostics-add-output=sarif with the defaults.  */

/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-add-output=sarif" } */

/* Verify that SARIF output can capture secondary locations
   relating to a diagnostic.  */

int missing_semicolon (void)
{
  return 42 /* { dg-error "expected ';' before '.' token" } */
}

/* Verify that JSON was written to the output file with the
   expected version and expected name:
   { dg-final { verify-sarif-file "2.1" }  } */
