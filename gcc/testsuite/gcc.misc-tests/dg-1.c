/* Test the tester.  */
/* { dg-prms-id 42 } */
/* { dg-do compile } */

main () { return 0; }

/* { dg-final { if ![file exists dg-1.s] { fail "dg-1.c (compile)"; return; } } } */

/* { dg-final { set tmp [grep dg-1.s main line]			} } */
/* { dg-final { if ![string match "" $tmp] \{			} } */
/* { dg-final {     pass "dg-1.c (main function present)"	} } */
/* { dg-final { \} else \{					} } */
/* { dg-final {     fail "dg-1.c (main function not present)"	} } */
/* { dg-final { \}						} } */
