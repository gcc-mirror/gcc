/* Regression test: #ifdef 0 should not crash.  Problem noted by
   Jakub Jelinek <jakub@redhat.com>.  */
/* { dg-do preprocess } */

#ifdef 0  /* { dg-error "with invalid argument" } */
#error not seen
#endif

#ifndef 0  /* { dg-error "with invalid argument" } */
#else
#error not seen
#endif
