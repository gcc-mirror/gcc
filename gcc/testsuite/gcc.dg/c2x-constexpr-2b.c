/* Test C2x constexpr.  Second file for link test.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* constexpr objects at file scope have internal linkage.  */
constexpr int a = 3;
