/* { dg-do compile } */
/* { dg-options "-Wall -std=gnu89" } */
/* This test should compile successfully.  */
extern inline int foo (void) { return 0; }
inline int foo (void) { return 1; }
