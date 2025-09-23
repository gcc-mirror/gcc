/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file -Wpedantic" } */

/* Verify SARIF output for a deletion fix-it hint.  */

struct foo
{
  int color;;  
};

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest extra-semicolon.c "extra-semicolon.py" } } */
