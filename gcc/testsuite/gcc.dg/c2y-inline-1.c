/* Test C2y constraint that inline functions with external linkage must be
   defined in same translation unit.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

inline void f (); /* { dg-error "declared but never defined" } */
extern inline void g(); /* { dg-error "declared but never defined" } */
