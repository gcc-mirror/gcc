/* PR middle-end/19857 */

typedef struct { char c[8]; } V
#ifdef __ELF__
  __attribute__ ((aligned (8)))
#endif
  ;
typedef __SIZE_TYPE__ size_t;
V v;
void abort (void);

int
main (void)
{
  V *w = &v;
  if (((size_t) ((float *) ((size_t) w & ~(size_t) 3)) % 8) != 0
      || ((size_t) w & 1))
    {
#ifndef __ELF__
      if (((size_t) &v & 7) == 0)
#endif
	abort ();
    }
  return 0;
}
