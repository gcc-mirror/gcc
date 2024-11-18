/* { dg-do compile } */
/* { dg-options "-fdiagnostics-add-output=sarif" } */

extern void foo (void);

void test_nesting (void)
{
  foo (); /* { dg-error "top-level error" } */
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest diagnostic-test-nesting-sarif.c "diagnostic-test-nesting-sarif.py" } } */
