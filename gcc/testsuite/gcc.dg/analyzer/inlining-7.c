/* Verify that we can reconstruct fndecl and stack depth information
   after early inlining.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */

/* We want the reconstructed call/return hierarchy to show
   that two calls happen at depth_3, without any spurious events
   popping the stack back any further.  */

static inline void
depth_6 (void *p)
{
  __builtin_free (p); /* { dg-warning "double-'free' of 'p1'" "warning" } */
 /* { dg-message "\\(7\\) first 'free' here \\(fndecl 'depth_6', depth 6\\)" "1st free message" { target *-*-* } .-1 } */
 /* { dg-message "\\(11\\) second 'free' here; first 'free' was at \\(7\\) \\(fndecl 'depth_6', depth 6\\)" "2nd free message" { target *-*-* } .-2 } */
}

static inline void
depth_5 (void *p5)
{
  depth_6 (p5); /* { dg-message "\\(6\\) inlined call to 'depth_6' from 'depth_5' \\(fndecl 'depth_5', depth 5\\)" "event 6" } */
  /* { dg-message "\\(10\\) inlined call to 'depth_6' from 'depth_5' \\(fndecl 'depth_5', depth 5\\)" "event 10" { target *-*-* } .-1 } */
}

static inline void
depth_4 (void *p4)
{
  depth_5 (p4); /* { dg-message "\\(5\\) inlined call to 'depth_5' from 'depth_4' \\(fndecl 'depth_4', depth 4\\)" "event 5" } */
  /* { dg-message "\\(9\\) inlined call to 'depth_5' from 'depth_4' \\(fndecl 'depth_4', depth 4\\)" "event 9" { target *-*-* } .-1 } */
}

static inline void
depth_3 (void *p3)
{
  depth_4 (p3); /* { dg-message "\\(4\\) inlined call to 'depth_4' from 'depth_3' \\(fndecl 'depth_3', depth 3\\)" } */
  depth_4 (p3); /* { dg-message "\\(8\\) inlined call to 'depth_4' from 'depth_3' \\(fndecl 'depth_3', depth 3\\)" } */
}

static inline void
depth_2 (void *p2)
{
  depth_3 (p2); /* { dg-message "\\(3\\) inlined call to 'depth_3' from 'depth_2' \\(fndecl 'depth_2', depth 2\\)" } */
}

void
depth_1 (void *p1) /* { dg-message "\\(1\\) entry to 'depth_1' \\(fndecl 'depth_1', depth 1\\)" } */
{
  depth_2 (p1); /* { dg-message "\\(2\\) inlined call to 'depth_2' from 'depth_1' \\(fndecl 'depth_1', depth 1\\)" } */
}
