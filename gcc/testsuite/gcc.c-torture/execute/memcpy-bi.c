/* Test builtin-memcpy (which may emit different code for different N).  */
#include <string.h>

void abort (void);

#define TESTSIZE 80

char src[TESTSIZE] __attribute__ ((aligned));
char dst[TESTSIZE] __attribute__ ((aligned));

void
check (char *test, char *match, int n)
{
  if (memcmp (test, match, n))
    abort ();
}

#define TN(n) \
{ memset (dst, 0, n); memcpy (dst, src, n); check (dst, src, n); }
#define T(n) \
TN (n) \
TN ((n) + 1) \
TN ((n) + 2) \
TN ((n) + 3)

int
main (void)
{
  int i,j;

  for (i = 0; i < sizeof (src); ++i)
      src[i] = 'a' + i % 26;

  T (0);
  T (4);
  T (8);
  T (12);
  T (16);
  T (20);
  T (24);
  T (28);
  T (32);
  T (36);
  T (40);
  T (44);
  T (48);
  T (52);
  T (56);
  T (60);
  T (64);
  T (68);
  T (72);
  T (76);

  return 0;
}
