/* Test that unterminated quotes in CPP expressions are
   recognized.  */

/* { dg-do preprocess } */

/* { dg-error "missing terminating" "bad charconst" { target *-*-* } 7 } */
#if 'x
#endif
