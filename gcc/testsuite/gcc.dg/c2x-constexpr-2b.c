/* Test C23 constexpr.  Second file for link test.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* constexpr objects at file scope have internal linkage.  */
constexpr int a = 3;
