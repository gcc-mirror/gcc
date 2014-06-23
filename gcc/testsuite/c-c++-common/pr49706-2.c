/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses" } */

/* Test that we don't warn if both operands of the comparison
   are negated.  */

#ifndef __cplusplus
#define bool _Bool
#endif

bool r;

int
same (int a, int b)
{
  r = !a == !b;
  r = !!a == !!b;
  r = !!a == !b;
  r = !a == !!b;
}
