/* { dg-do preprocess } */

/* Test the ? : opearator, for precedence and both true and false.  */

#if  1 ? 1 ? 2 : 0 : 0
#error OK	/* { dg-error "OK" "nested ? :" } */
#endif

#if  ((0) ? (1) ? (2) : (3) : (4) ? (5): (6)) == 5
#error OK	/* { dg-error "OK" "nested ? : with parens" } */
#endif


