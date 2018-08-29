/* { dg-do compile } */
/* { dg-options "-O2 -w -fdump-tree-dom2-details" } */

#include <string.h>

typedef struct string_s {
  unsigned long size, alloc;
  char *ptr;
} string_t[1];

# define M_ASSUME(x)                                    \
  (! __builtin_constant_p (!!(x) || !(x)) || (x) ?      \
   (void) 0 : __builtin_unreachable())

int f(string_t s)
{
  M_ASSUME(strlen(s->ptr) == s->size);
  return s->size;
}

/* { dg-final { scan-assembler-not "strlen" } } */

