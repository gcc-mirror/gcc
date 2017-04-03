/* { dg-do compile } */
/* { dg-skip-if "ARC600 doesn't support pic" { arc6xx } } */
/* { dg-options "-Os -fPIC" } */
#include <stdio.h>
#include <string.h>

char *
strip_trail (const char str[], size_t n)
{
  static char buf[1025];
  int j;

  strncpy (buf, str, n);
  buf[n] = '\0';

  for (j = strlen (buf) - 1; j >= 0; j--)
    {
      if (buf[j] != ' ')
        break;

      buf[j] = '\0';
    }

  return buf;
}
