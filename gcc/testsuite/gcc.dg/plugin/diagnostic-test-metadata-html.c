/* { dg-do compile } */
/* { dg-options "-fdiagnostics-set-output=experimental-html:javascript=no" } */
/* { dg-additional-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

extern char *gets (char *s);

void test_cwe (void)
{
  char buf[1024];
  gets (buf);
}

/* Use a Python script to verify various properties about the generated
   HTML file:
   { dg-final { run-html-pytest diagnostic-test-metadata-html.c "diagnostic-test-metadata-html.py" } } */
