/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

extern char *gets (char *s);

void test_cwe (void)
{
  char buf[1024];
  gets (buf);
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest diagnostic-test-metadata-sarif.c "diagnostic-test-metadata-sarif.py" } } */
