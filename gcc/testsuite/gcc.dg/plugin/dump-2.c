/* { dg-do compile } */
/* { dg-options "-fopt-info-loop-note-internals" } */

extern void test_string_literal (void);
extern void test_tree (void);
extern void test_gimple (int);
extern void test_cgraph_node (void);
extern void test_wide_int (void);
extern void test_poly_int (void);
extern void test_scopes (void);

void test_remarks (void)
{
  test_string_literal (); /* { dg-message "test of dump for 'test_string_literal'" } */
  test_tree (); /* { dg-message "test of tree: 0" } */
  test_gimple (42); /* { dg-message "test of gimple: test_gimple \\(42\\);" } */
  test_cgraph_node (); /* { dg-message "test of callgraph node: test_cgraph_node/\[0-9\]+" } */
  test_wide_int (); /* { dg-message "test of wide int: 0" } */
  test_poly_int (); /* { dg-message "test of poly int: 42" } */

  /* Dump messages in nested scopes are not printed by default, and
     require "-internals".  */
  test_scopes (); /* { dg-line test_scopes_line } */
  /* { dg-message "=== outer scope ===" "" { target *-*-* } test_scopes_line } */
  /* { dg-message " at outer scope" "" { target *-*-* } test_scopes_line } */
  /* { dg-message " === middle scope ===" "" { target *-*-* } test_scopes_line } */
  /* { dg-message "  at middle scope" "" { target *-*-* } test_scopes_line } */
  /* { dg-message "  === innermost scope ===" "" { target *-*-* } test_scopes_line } */
  /* { dg-message "   at innermost scope" "" { target *-*-* } test_scopes_line } */
}
