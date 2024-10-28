/* { dg-do compile } */
/* { dg-options "-Werror=unused-variable -fdiagnostics-format=sarif-file" } */

/* Verify our SARIF output for a translation unit with -Werror.  */

static int ununsed;

/* We expect a failing compile due to the Werror, but the use of 
   -fdiagnostics-format=sarif-file means there should be no output to stderr.
   DejaGnu injects this message; ignore it:
   { dg-prune-output "exit status is 1" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest werror.c "werror.py" } } */
