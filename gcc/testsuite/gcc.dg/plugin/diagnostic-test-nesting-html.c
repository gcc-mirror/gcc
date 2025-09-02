/* { dg-do compile } */
/* { dg-options "-fdiagnostics-add-output=experimental-html:javascript=no" } */

extern void foo (void);

void test_nesting (void)
{
  foo (); /* { dg-error "top-level error" } */
}

/* Use a Python script to verify various properties about the generated
   .html file:
   { dg-final { run-html-pytest diagnostic-test-nesting-html.c "diagnostic-test-nesting-html.py" } } */
