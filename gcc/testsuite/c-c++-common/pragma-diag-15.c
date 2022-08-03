/* { dg-do compile } */
/* { dg-additional-options "-Wunused-macros" } */

/* In the past, the pragma has erroneously disabled the warning because the
   location was not tracked properly with -E or -save-temps; check that it works
   now.

   This test currently fails for C++ but it's not because of the pragma, it's
   because the location of the macro definition is incorrectly set.  This is a
   separate issue, will resolve it in a later patch.  */

#define X /* { dg-warning "-Wunused-macros" } */
#pragma GCC diagnostic ignored "-Wunused-macros"
