/* { dg-do run { target spu-*-* } } */
/* { dg-require-effective-target "ea32" } */
/* { dg-options "-std=gnu99 -O2 -mea32" } */

extern void abort (void);
extern unsigned long long __ea_local_store;

__ea int *ppu;
int x, *spu = &x, *spu2;

int
main (int argc, char **argv)
{
  ppu = (__ea int *) spu;
  spu2 = (int *) ppu;

#ifdef __EA32__
  if ((int) ppu != (int) __ea_local_store + (int) spu)
#else
  if ((unsigned long long) ppu != __ea_local_store + (unsigned long long)(int) spu)
#endif

    abort ();

  if (spu != spu2)
    abort ();

  return 0;
}
