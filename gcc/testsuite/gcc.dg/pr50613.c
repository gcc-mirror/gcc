/* PR tree-optimization/50613 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp" } */

#include "strlenopt.h"

char buf[26];

static inline void
bar (char *__restrict dest, const char *__restrict src)
{
  strcpy (dest, src);
}

void
foo (char *p)
{
  if (strlen (p) < 50)
    bar (buf, p);
}
