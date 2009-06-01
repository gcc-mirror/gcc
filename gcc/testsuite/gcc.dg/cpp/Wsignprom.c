/* { dg-do preprocess } */
/* { dg-options "-Wall" } */

/* Test that -Wall emits the warnings about integer promotion changing
   the sign of an operand.  */

#if -1 > 0U  /* { dg-warning "5:changes sign when promoted" } */
#endif

#if 0U + -1  /* { dg-warning "10:changes sign when promoted" } */
#endif

#if 0U * -1  /* { dg-warning "10:changes sign when promoted" } */
#endif

#if 1U / -2  /* { dg-warning "10:changes sign when promoted" } */
#endif

#if -1 % 1U  /* { dg-warning "5:changes sign when promoted" } */
#endif

#if 1 ? 0U : -1  /* { dg-warning "14:changes sign when promoted" } */
#endif

#if 1 ? -1 : 0U  /* { dg-warning "9:changes sign when promoted" } */
#endif
