/* An over-aligned scalar parameter whose address is taken.  */

/* { dg-do run } */
/* { dg-options "-O2" } */

#define ALIGNMENT 64

typedef int V __attribute__((aligned (ALIGNMENT)));

void check (V *) __attribute__((noipa));
void two (V, V) __attribute__((noipa));

void
check (V *p)
{
  if ((__UINTPTR_TYPE__) p % ALIGNMENT != 0)
    __builtin_abort ();
}

/* Two parameters, in case one is sufficiently aligned by chance.  */
void
two (V x, V y)
{
  check (&x);
  check (&y);
}

int
main (void)
{
  two (-43, 29);
  return 0;
}
