/* Verify that we can output multiple different versions of SARIF
   with -fdiagnostics-add-output (specifying version and filename),
   as well as usual text output.  */

/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-add-output=sarif:file=multiple-outputs-c.2.1.sarif,version=2.1" } */
/* { dg-additional-options "-fdiagnostics-add-output=sarif:file=multiple-outputs-c.2.2.sarif,version=2.2-prerelease" } */

/* Verify that SARIF output can capture secondary locations
   relating to a diagnostic.  */

int missing_semicolon (void)
{
  return 42 /* { dg-error "expected ';' before '.' token" } */
}

/* Verify that JSON was written to the output files with the
   expected version and expected names:
   { dg-final { verify-sarif-file "2.1" "multiple-outputs-c.2.1.sarif" }  }
   { dg-final { verify-sarif-file "2.2" "multiple-outputs-c.2.2.sarif" }  }
*/

/* Use a Python script to verify various properties about the generated
   .sarif files:
   { dg-final { run-sarif-pytest multiple-outputs-c.2.1 "multiple-outputs.py" } }
   { dg-final { run-sarif-pytest multiple-outputs-c.2.2 "multiple-outputs.py" } }
*/
