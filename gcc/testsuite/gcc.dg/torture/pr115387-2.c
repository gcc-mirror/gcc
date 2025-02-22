/* Test there is no ICE when compile.  */
/* { dg-do compile } */

#include <stddef.h>
#include <stdint.h>

char *
test (char *string, size_t maxlen)
{
  string[0] = '\0';
  uintptr_t end;

  if (__builtin_add_overflow ((uintptr_t) string, maxlen, &end))
    end = -1;

  return (char *) end;
}
