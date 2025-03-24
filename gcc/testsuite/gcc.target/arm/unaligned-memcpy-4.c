/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */

#include <string.h>

char src[16] __attribute__ ((aligned(8))) = "abcdefghijklmnop";
char dest[16] __attribute__ ((aligned(8))) = { 0 };

void __attribute__ ((noinline,noclone))
aligned_both (void)
{
  memcpy (dest, src, 16);
}

int main ()
{
  int i;
  aligned_both ();
  for (i = 0; i < 16; i++)
    if (dest[i] != src[i])
      __builtin_abort ();
  return 0;
}

/* There should be no 'unaligned' comments.  */
/* { dg-final { scan-assembler-not "@ unaligned" } } */
