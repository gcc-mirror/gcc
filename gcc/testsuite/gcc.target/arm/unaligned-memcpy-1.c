/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */

#include <string.h>

char src[17] __attribute__ ((aligned(8))) = "abcdefghijklmnopq";
char result[17] __attribute__ ((aligned(8))) = {0};

void __attribute__ ((noinline,noclone))
unknown_alignment (char *dest, char *src)
{
  memcpy (dest, src, 15);
}

int main ()
{
  int i;
  unknown_alignment (result+1, src+2);
  for (i = 0; i < 15; i++)
    if (result[i+1] != src[i+2])
      __builtin_abort ();
  if (result[16] != 0)
    __builtin_abort ();
  return 0;
}

/* Check that we don't use any instructions that assume an aligned source.  */
/* { dg-final { scan-assembler-not {(ldm(ia)?\tr[0-9]|ldrd\t.*\[r[0-9]|vldr)} } } */

/* Check that we don't use any instructions that assume an aligned dest.  */
/* { dg-final { scan-assembler-not {(stm(ia)?\tr[0-9]|strd\t.*\[r[0-9]|vstr)} } } */
