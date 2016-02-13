/* { dg-additional-options "-std=gnu99 -fPIC" }  */
/* { dg-do compile }  */

#pragma GCC visibility push(hidden)

double _Complex foo (double _Complex arg);

double _Complex
bar (double _Complex arg)
{
  return foo (arg);
}
