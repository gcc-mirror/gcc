/* Test for -fno-analyzer-undo-inlining.
   Verify that we can disable reconstruction of fndecl and stack depth
   information.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths -fno-analyzer-undo-inlining" } */

void foo (void *p)
{
  __builtin_free (p); /* { dg-warning "double-'free' of 'q'" "warning" } */
  /* { dg-message "\\(1\\) first 'free' here \\(fndecl 'bar', depth 1\\)" "1st free message" { target c } .-1 } */
  /* { dg-message "\\(1\\) first 'free' here \\(fndecl 'void bar\\(void\\*\\)', depth 1\\)" "1st free message" { target c++ } .-2 } */
  /* { dg-message "\\(2\\) second 'free' here; first 'free' was at \\(1\\) \\(fndecl 'bar', depth 1\\)" "2nd free message" { target c } .-3 } */
  /* { dg-message "\\(2\\) second 'free' here; first 'free' was at \\(1\\) \\(fndecl 'void bar\\(void\\*\\)', depth 1\\)" "2nd free message" { target c++ } .-4 } */
}

void bar (void *q)
{
  foo (q);
  foo (q);
}
