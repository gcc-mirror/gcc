/* Test C23 static assertions.  static_assert keyword.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

static_assert (1);
static_assert (1, "message");
