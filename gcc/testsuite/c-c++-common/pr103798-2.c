/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized -save-temps" } */

#include <string.h>

__attribute__ ((weak))
int
f (int a)
{
   return memchr ("aE", a, 2) != NULL;
}

__attribute__ ((weak))
int
g (char a)
{
  return a == 'a' || a == 'E';
}

int
main ()
{
 for (int i = 0; i < 255; i++)
   if (f (i + 256) != g (i + 256))
     __builtin_abort ();

 return 0;
}

/* { dg-final { scan-assembler-not "memchr" } } */
