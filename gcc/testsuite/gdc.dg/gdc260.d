// https://bugzilla.gdcproject.org/show_bug.cgi?id=260
// { dg-options "-Wall -Werror" }
// { dg-do compile }

import gcc.builtins;

char *bug260(char *buffer)
{
  return __builtin_strcat(&buffer[0], "Li");
}
