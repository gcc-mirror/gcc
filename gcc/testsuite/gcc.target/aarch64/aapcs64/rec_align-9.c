/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target aarch64*-*-* } } */

extern int memcmp (const void *s1, const void *s2, __SIZE_TYPE__ n);
extern void abort (void);

struct s
  {
    /* This forces the alignment and size of the struct to 16.  */
    __attribute__ ((__aligned__ (16))) long x;
    int y;
    /* 4 bytes padding.  */
  };

typedef struct s __attribute__ ((__aligned__ (8))) underaligned;

underaligned a = { 1, 4 };
underaligned b = { 9, 16 };
underaligned c = { 25, 36 };

void
test_underaligned_struct (int x0, underaligned x2, int x4, underaligned x6,
			  int stack, underaligned stack16)
{
  if (x0 != 3 || x4 != 5 || stack != 7)
    abort ();
  if (memcmp ((void *) &x2, (void *)&a, sizeof (underaligned)))
    abort ();
  if (memcmp ((void *)&x6, (void *)&b, sizeof (underaligned)))
    abort ();
  if (memcmp ((void *)&stack16, (void *)&c, sizeof (underaligned)))
    abort ();
}

int
main (int argc, char **argv)
{
  test_underaligned_struct (3, a, 5, b, 7, c);
  return 0;
}
