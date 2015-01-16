/* { dg-do compile } */
/* { dg-options "-O -mcpu=gr6" } */

extern void abort (void);

#define LEN 256

void foo (void)
{
  int dst[LEN], src[LEN];
  unsigned int i;

  __builtin_memset (src, 0, LEN * sizeof (int));
  __builtin_memcpy (dst, src, LEN * sizeof (int));
  if (__builtin_memcmp (dst, src, LEN * sizeof (int)) != 0)
    abort ();
}

/* { dg-final { scan-assembler "bmd" } } */
