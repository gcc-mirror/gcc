/* Verify that we can reconstruct fndecl and stack depth information
   after early inlining.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */

static inline void
inner (void *p)
{
  __builtin_free (p); /* { dg-warning "double-'free' of 'r'" } */
  /* { dg-message "\\(5\\) second 'free' here; first 'free' was at \\(3\\) \\(fndecl 'inner', depth 3\\)" "2nd free message" { target c } .-1 } */
  /* { dg-message "\\(5\\) second 'free' here; first 'free' was at \\(3\\) \\(fndecl 'void inner\\(void\\*\\)', depth 3\\)" "2nd free message" { target c++ } .-2 } */
}

static inline void
middle (void *q)
{
  __builtin_free (q); /* { dg-message "\\(3\\) first 'free' here \\(fndecl 'middle', depth 2\\)" "1st free message" { target c } } */
  /* { dg-message "\\(3\\) first 'free' here \\(fndecl 'void middle\\(void\\*\\)', depth 2\\)" "1st free message" { target c++ } .-1 } */
  inner (q); /* { dg-message "\\(4\\) inlined call to 'inner' from 'middle' \\(fndecl 'middle', depth 2\\)" "" { target c } } */
  /* { dg-message "\\(4\\) inlined call to 'inner' from 'middle' \\(fndecl 'void middle\\(void\\*\\)', depth 2\\)" "" { target c++ } .-1 } */
}

void
outer (void *r) /* { dg-message "\\(1\\) entry to 'outer' \\(fndecl 'outer', depth 1\\)" "" { target c } } */
/* { dg-message "\\(1\\) entry to 'outer' \\(fndecl 'void outer\\(void\\*\\)', depth 1\\)" "" { target c++ } .-1 } */
{
  middle (r); /* { dg-message "\\(2\\) inlined call to 'middle' from 'outer' \\(fndecl 'outer', depth 1\\)" "" { target c } } */
  /* { dg-message "\\(2\\) inlined call to 'middle' from 'outer' \\(fndecl 'void outer\\(void\\*\\)', depth 1\\)" "" { target c++ } .-1 } */
}
