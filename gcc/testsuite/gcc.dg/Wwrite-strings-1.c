/* Test pointer initialization and dereference don't lose qualifiers
   on array types.  This test wrongly failed to diagnose the loss of
   const.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wwrite-strings" } */
typedef char T[1];
T *p = &""; /* { dg-warning "initialization from incompatible pointer type" } */
