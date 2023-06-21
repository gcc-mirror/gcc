/* { dg-do preprocess } */
/* { dg-additional-options "-Wunused-macros" } */

/* In the past, the pragma has erroneously disabled the warning because the
   location was not tracked properly with -E or -save-temps; check that it works
   now.  */

#define X /* { dg-warning "-Wunused-macros" } */
#pragma GCC diagnostic ignored "-Wunused-macros"
