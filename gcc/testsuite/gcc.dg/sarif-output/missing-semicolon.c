/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

/* Verify that SARIF output can capture secondary locations
   relating to a diagnostic.  */

int missing_semicolon (void)
{
  return 42
}

/* We expect a failing compile due to the error, but the use of 
   -fdiagnostics-format=sarif-file means there should be no output to stderr.
   DejaGnu injects this message; ignore it:
   { dg-prune-output "exit status is 1" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest missing-semicolon.c "missing-semicolon.py" } } */
