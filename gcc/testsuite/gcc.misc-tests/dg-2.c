/* Test the tester.  */
/* { dg-prms-id 42 } */
/* { dg-do assemble } */

main () { return 0; }

/* { dg-final { if [file exists dg-2.o] \{			} } */
/* { dg-final {     pass "dg-2.c (assemble: produce .o test)"	} } */
/* { dg-final { \} else \{					} } */
/* { dg-final {     fail "dg-2.c (assemble: produce .o test)"	} } */
/* { dg-final { \}						} } */
