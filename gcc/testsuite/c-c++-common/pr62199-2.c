/* PR c++/62199 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses" } */

#ifndef __cplusplus
# define bool _Bool
#endif

bool r;

void
foo (bool b)
{
  r = !b == 1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  r = !b != 1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  r = !b > 1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  r = !b >= 1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  r = !b < 1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
  r = !b <= 1; /* { dg-bogus "logical not is only applied to the left hand side of comparison" } */
}
