/* Test the tester; previously gcc.misc-tests/dg-3.c.  */
/* { dg-prms-id 42 } */
/* { dg-do link } */

main () { return 0; }

/* { dg-final { if [file exists dg-do-link-exp-P.exe] \{			} } */
/* { dg-final {     pass "dg-do-link-exp-P.c (link: produce a.out test)"	} } */
/* { dg-final { \} else \{					} } */
/* { dg-final {     fail "dg-do-link-exp-P.c (link: produce a.out test)"	} } */
/* { dg-final { \}						} } */
