/* Test that unterminated quotes in CPP expressions are
   recognized.  */

/* { dg-do preprocess } */

/* { dg-error "not valid" "bad charconst" { target *-*-* } 7 } */
#if 'x
#endif
