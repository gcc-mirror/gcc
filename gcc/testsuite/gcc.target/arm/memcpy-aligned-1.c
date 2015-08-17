/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

void *memcpy (void *dest, const void *src, unsigned int n);

void foo (char *dst, int i)
{
  memcpy (dst, &i, sizeof (i));
}

/* { dg-final { scan-assembler-times "str\t" 1 } } */
/* { dg-final { scan-assembler-not "ldr\t" } } */
