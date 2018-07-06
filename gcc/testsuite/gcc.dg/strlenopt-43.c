/* PR tree-optimization/83896 - ice in get_string_len on a call to strlen
   with non-constant length
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

extern char a[5];
extern char b[];

void f (void)
{
  if (strlen (b) != 4)
    memcpy (a, b, sizeof a);
}
