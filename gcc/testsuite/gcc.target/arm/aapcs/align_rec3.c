/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2 -fno-inline" } */

/* Test AAPCS layout (alignment) for callee.  */

extern int memcmp (const void *s1, const void *s2, __SIZE_TYPE__ n);
extern void abort (void);


/* Struct will be aligned to 8.  */
struct s
  {
    int x;
    /* 4 bytes padding here.  */
    __attribute__((aligned (8))) int y;
    /* 4 bytes padding here.  */
  };

typedef struct s __attribute__((aligned (4))) underaligned;

underaligned a = { 1, 4 };
underaligned b = { 9, 16 };

void
f (int r0, underaligned r2, int stack8, underaligned stack16)
{
  if (r0 != 3 || stack8 != 6)
    abort ();
  if (memcmp ((void *) &r2, (void *)&a, sizeof (underaligned)))
    abort ();
  if (memcmp ((void *)&stack16, (void *)&b, sizeof (underaligned)))
    abort ();
}

int
main (int argc, char **argv)
{
  f (3, a, 6, b);
  return 0;
}
