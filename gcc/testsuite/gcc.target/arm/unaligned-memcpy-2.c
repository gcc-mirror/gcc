/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */

#include <string.h>

char dest[16] __attribute__((aligned(8))) = { 0 } ;
char input[17] __attribute__ ((aligned(8))) = "abcdefghijklmnop";

void __attribute__ ((noinline,noclone)) aligned_dest (char *src)
{
  memcpy (dest, src, 15);
}

int main ()
{
  int i;
  aligned_dest (input+1);
  for (i = 0; i < 15; i++)
    if (dest[i] != input[i+1])
      __builtin_abort ();
  if (dest[15] != 0)
    __builtin_abort ();
  return 0;
}

/* Check that we don't use any instructions that assume an aligned source.  */
/* { dg-final { scan-assembler-not {(ldm(ia)?\tr[0-9]|ldrd\t.*\[r[0-9]|vldr)} } } */
