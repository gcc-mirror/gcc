/* Test of the "no diagnostics are emitted" case.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

int
main ()
{
  begin_test ("test-no-diagnostics.c.exe",
	      "test-no-diagnostics.c.sarif",
	      __FILE__, "c");

  /* No-op.  */

  return end_test ();
};

/* There should be no output from the text sink.  */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-no-diagnostics.c "test-no-diagnostics-c.py" } } */
