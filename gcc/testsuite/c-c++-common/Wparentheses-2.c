// PR c++/95344 - bogus -Wparentheses warning.
// { dg-do compile }
// { dg-options "-Wparentheses" }

#ifndef __cplusplus
# define bool _Bool
# define true 1
# define false 0
#endif

void
f (int i)
{
  bool b = false;
  if (i == 99 ? (b = true) : false) // { dg-bogus "suggest parentheses" }
    {
    }
}
