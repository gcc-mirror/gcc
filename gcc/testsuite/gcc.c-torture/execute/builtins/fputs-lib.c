#include <stdio.h>
#include <stddef.h>
extern int inside_main;
extern size_t strlen(const char *);
int
fputs(const char *string, FILE *stream)
{
  size_t n = strlen(string);
  size_t r;
#if defined __OPTIMIZE__ && !defined __OPTIMIZE_SIZE__
  if (inside_main)
    abort();
#endif
  r = fwrite (string, 1, n, stream);
  return n > r ? EOF : 0;
}

