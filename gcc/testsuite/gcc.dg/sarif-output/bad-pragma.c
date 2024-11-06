/* Verify that SARIF output can capture URLs in diagnostics
   related to a bad pragma.  */

/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file -Wpragmas" } */

#pragma GCC diagnostic ignored "-Wmisleading-indenttion"

int nonempty;

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest bad-pragma.c "bad-pragma.py" } } */
