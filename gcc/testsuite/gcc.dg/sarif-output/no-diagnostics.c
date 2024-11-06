/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

/* Verify our SARIF output for a translation unit with no diagnostics.  */

int nonempty;

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest no-diagnostics.c "no-diagnostics.py" } } */
