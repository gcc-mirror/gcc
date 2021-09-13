#include <stdio.h>
#ifndef GCC_TMPNAM
#define GCC_TMPNAM
static inline char *gcc_tmpnam(char *s)
{
  char *ret = tmpnam (s);
  // Windows sometimes prepends a backslash to denote the current directory,
  // so swallow that if it occurs
  if (ret[0] == '\\')
    ++ret;
  return ret;
}
#endif
