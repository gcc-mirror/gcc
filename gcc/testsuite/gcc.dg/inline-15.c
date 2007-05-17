/* Check that an error message is produced when a C99 inline function
   is never defined.  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

extern inline int func1 (void); /* { dg-warning "never defined" } */
inline int func2 (void); /* { dg-warning "never defined" } */
