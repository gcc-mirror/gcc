/* Test the tester.  */
/* { dg-prms-id 42 } */
/* { dg-do link } */

main () { return 0; }

/* { dg-final { if [file exists a.out] \{			} } */
/* { dg-final {     pass "dg-2.c (link: produce a.out test)"	} } */
/* { dg-final { \} else \{					} } */
/* { dg-final {     fail "dg-2.c (link: produce a.out test)"	} } */
/* { dg-final { \}						} } */
