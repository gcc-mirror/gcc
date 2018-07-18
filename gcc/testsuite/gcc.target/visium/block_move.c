/* { dg-do compile } */
/* { dg-skip-if "no bmd in gr5" { "visium*-*-*" } { "-mcpu=gr5" } { "" } } */
/* { dg-options "-O -mcpu=gr6" } */

extern void abort (void);

#define LEN 256

void foo (void)
{
  int dst[LEN], src[LEN];
  unsigned int i;

  __builtin_memset (src, 1, LEN * sizeof (int));
  __builtin_memcpy (dst, src, LEN * sizeof (int));
  if (__builtin_memcmp (dst, src, LEN * sizeof (int)) != 0)
    abort ();
}

/* { dg-final { scan-assembler "bmd" } } */
