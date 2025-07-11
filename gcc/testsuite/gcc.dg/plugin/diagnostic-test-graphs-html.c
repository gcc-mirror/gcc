/* { dg-do compile } */
/* { dg-options "-fdiagnostics-add-output=experimental-html:javascript=no" } */

extern void here (void);

void test_graphs (void)
{
  here (); /* { dg-error "this is a placeholder error, with graphs" } */
}

/* Use a Python script to verify various properties about the generated
   HTML file:
   { dg-final { run-html-pytest diagnostic-test-graphs-html.c "diagnostic-test-graphs-html.py" } } */
