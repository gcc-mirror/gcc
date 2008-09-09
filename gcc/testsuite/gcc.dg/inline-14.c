/* Check that you can't redefine a C99 inline function.  */
/* { dg-do compile } */
/* { dg-options "-std=c99" } */

extern inline int func1 (void) /* { dg-message "note: previous definition" } */
{
  return 1;
}

inline int func1 (void) /* { dg-error "redefinition" } */
{
  return 1;
}

inline int func2 (void) /* { dg-message "note: previous definition" } */
{
  return 2;
}

inline int func2 (void) /* { dg-error "redefinition" } */
{
  return 2;
}
