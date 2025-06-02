/* { dg-do compile } */
/* { dg-options "-fdiagnostics-add-output=experimental-html:javascript=no" } */

/* Verify that basics of HTML output work.  */

int missing_semicolon (void)
{
  return 42 /* { dg-error "expected ';' before '.' token" } */
}

/* Use a Python script to verify various properties about the generated
   .html file:
   { dg-final { run-html-pytest missing-semicolon.c "missing-semicolon.py" } } */
