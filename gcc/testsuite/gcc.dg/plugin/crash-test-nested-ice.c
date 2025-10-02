/* { dg-do compile } */
/* { dg-additional-options "-fno-report-bug" } */
/* { dg-additional-options "-fplugin-arg-crash_test_plugin-nested" } */
/* { dg-additional-options "-fdiagnostics-add-output=sarif" } */
/* { dg-additional-options "-fdiagnostics-add-output=experimental-html:javascript=no" } */

extern void inject_ice (void);

void test_1 (void)
{
  inject_ice ();   /* { dg-ice "I'm sorry Dave, I'm afraid I can't do that" } */
  /* { dg-error "placeholder" "" { target *-*-* } .-1 } */
  /* { dg-regexp "during GIMPLE pass: crash_test" } */
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest crash-test-nested-ice.c "crash-test-nested-ice-sarif.py" } } */

/* Use a Python script to verify various properties about the generated
   .html file:
   { dg-final { run-html-pytest crash-test-nested-ice.c "crash-test-nested-ice-html.py" } } */
