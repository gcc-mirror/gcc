/* Verify that we can reconstruct fndecl and stack depth information
   after early inlining.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */

void foo (void *p)
{
  __builtin_free (p); /* { dg-warning "double-'free' of 'q'" "warning" } */
  /* { dg-message "\\(3\\) first 'free' here \\(fndecl 'foo', depth 2\\)" "1st free message" { target *-*-* } .-1 } */
  /* { dg-message "\\(5\\) second 'free' here; first 'free' was at \\(3\\) \\(fndecl 'foo', depth 2\\)" "2nd free message" { target *-*-* } .-2 } */
}

void bar (void *q) /* { dg-message "\\(1\\) entry to 'bar' \\(fndecl 'bar', depth 1\\)" } */
{
  foo (q); /* { dg-message "\\(2\\) inlined call to 'foo' from 'bar' \\(fndecl 'bar', depth 1\\)" } */
  foo (q); /* { dg-message "\\(4\\) inlined call to 'foo' from 'bar' \\(fndecl 'bar', depth 1\\)" } */
}
