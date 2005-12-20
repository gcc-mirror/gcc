/* Test the tester; previously gcc.misc-tests/dg-2.c.  */
/* { dg-prms-id 42 } */
/* { dg-do assemble } */

main () { return 0; }

/* { dg-final { if [file exists dg-do-assemble-exp-P.o] \{	} } */
/* { dg-final {     pass "dg-do-assemble-exp-P.c (assemble: produce .o test)"	} } */
/* { dg-final { \} else \{					} } */
/* { dg-final {     fail "dg-do-assemble-exp-P.c (assemble: produce .o test)"	} } */
/* { dg-final { \}						} } */
