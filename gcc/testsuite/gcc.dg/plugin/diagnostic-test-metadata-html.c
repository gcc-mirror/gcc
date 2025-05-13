/* { dg-do compile } */
/* { dg-options "-fdiagnostics-set-output=experimental-html" } */
/* { dg-additional-options "-fdiagnostics-show-caret" } */

extern char *gets (char *s);

void test_cwe (void)
{
  char buf[1024];
  gets (buf);
}

/* Use a Python script to verify various properties about the generated
   HTML file:
   { dg-final { run-html-pytest diagnostic-test-metadata-html.c "diagnostic-test-metadata-html.py" } } */
