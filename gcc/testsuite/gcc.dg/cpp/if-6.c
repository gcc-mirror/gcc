/* { dg-do preprocess } */
#ifdef foo bar  /* { dg-error "extra tokens" "tokens after #ifdef" } */
#endif
