/* Verify basic C99 inline functionality.  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

static inline int func2 (void)
{
}

inline int dontgenerate1 (void)
{ /* { dg-warning "inline" "" } */
  func2 ();
  return 1;
}

extern inline int func1 (void) { return 1; }  /* { dg-warning "inline" "" } */
