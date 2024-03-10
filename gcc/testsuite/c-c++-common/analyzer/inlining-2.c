/* Verify that we can reconstruct fndecl and stack depth information
   after early inlining.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */

static void __analyzer_foo (void *p)
{
  __builtin_free (p); /* { dg-message "\\(3\\) first 'free' here \\(fndecl '__analyzer_foo', depth 2\\)" "1st free message" { target c } } */
  /* { dg-message "\\(3\\) first 'free' here \\(fndecl 'void __analyzer_foo\\(void\\*\\)', depth 2\\)" "1st free message" { target c++ } .-1 } */

  __builtin_free (p); /* { dg-warning "double-'free' of 'q'" "warning" } */
  /* { dg-message "\\(4\\) second 'free' here; first 'free' was at \\(3\\) \\(fndecl '__analyzer_foo', depth 2\\)" "2nd free message" { target c } .-1 } */
  /* { dg-message "\\(4\\) second 'free' here; first 'free' was at \\(3\\) \\(fndecl 'void __analyzer_foo\\(void\\*\\)', depth 2\\)" "2nd free message" { target c++ } .-2 } */
}

void bar (void *q) /* { dg-message "\\(1\\) entry to 'bar' \\(fndecl 'bar', depth 1\\)" "" { target c } } */
/* { dg-message "\\(1\\) entry to 'bar' \\(fndecl 'void bar\\(void\\*\\)', depth 1\\)" "" { target c++ } .-1 } */
{
  __analyzer_foo (q); /* { dg-message "\\(2\\) inlined call to '__analyzer_foo' from 'bar' \\(fndecl 'bar', depth 1\\)" "" { target c } } */
  /* { dg-message "\\(2\\) inlined call to '__analyzer_foo' from 'bar' \\(fndecl 'void bar\\(void\\*\\)', depth 1\\)" "" { target c++ } .-1 } */
}
