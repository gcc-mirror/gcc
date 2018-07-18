/* Test that unterminated quotes in CPP expressions are
   recognized.  */

/* { dg-do preprocess } */
/* { dg-warning "-:missing terminating" "bad charconst" { target *-*-* } .+2 } */
/* { dg-error "-:not valid" "bad charconst" { target *-*-* } .+1 } */
#if 'x
#endif
