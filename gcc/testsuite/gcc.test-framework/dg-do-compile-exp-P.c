/* Test the tester; previously gcc.misc-tests/dg-1.c.  */
/* { dg-prms-id 42 } */
/* { dg-do compile } */

main () { return 0; }

/* { dg-final { if ![file exists dg-do-compile-exp-P.s] { fail "dg-do-compile-exp-P.c (compile)"; return; } } } */

/* { dg-final { set tmp [grep dg-do-compile-exp-P.s main line]			} } */
/* { dg-final { if ![string match "" $tmp] \{			} } */
/* { dg-final {     pass "dg-do-compile-exp-P.c (main function present)"	} } */
/* { dg-final { \} else \{					} } */
/* { dg-final {     fail "dg-do-compile-exp-P.c (main function not present)"	} } */
/* { dg-final { \}						} } */
