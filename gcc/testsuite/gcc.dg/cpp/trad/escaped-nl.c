/* Test escaped newlines at start of macro definition are properly
   skipped (buglet in skip_whitespace () in cpptrad.c).  */

/* { dg-do preprocess } */

#define NUM \
100
#if NUM != 100
# error NUM not defined properly /* { dg-bogus "error" } */
#endif
