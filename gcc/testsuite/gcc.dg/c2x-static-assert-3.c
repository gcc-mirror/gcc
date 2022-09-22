/* Test C2x static assertions.  static_assert keyword.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic" } */

static_assert (1);
static_assert (1, "message");
