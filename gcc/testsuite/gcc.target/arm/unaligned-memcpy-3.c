/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */

#include <string.h>

char src[17] __attribute__ ((aligned(8))) = "abcdefghijklmnop";
char result[17] __attribute__ ((aligned(8))) = {0};

void __attribute__ ((noinline,noclone)) aligned_src (char *dest)
{
  memcpy (dest, src, 15);
}

int main ()
{
  int i;
  aligned_src (result+1);
  for (i = 0; i < 15; i++)
    if (result[i+1] != src[i])
      __builtin_abort ();
  if (result[16] != 0)
    __builtin_abort ();
  return 0;
}

/* Check that we don't use any instructions that assume an aligned dest.  */
/* { dg-final { scan-assembler-not {(stm(ia)?\tr[0-9]|strd\t.*\[r[0-9]|vstr)} } } */
