/* { dg-do compile } */
/* { dg-options "-fdiagnostics-add-output=sarif" } */

extern void here (void);

void test_graphs (void)
{
  here (); /* { dg-error "this is a placeholder error, with graphs" } */
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest diagnostic-test-graphs-sarif.c "diagnostic-test-graphs-sarif.py" } } */
