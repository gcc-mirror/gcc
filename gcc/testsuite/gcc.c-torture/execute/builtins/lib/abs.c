extern int inside_main;
extern void abort (void);
#ifdef __OPTIMIZE__
#define ABORT_INSIDE_MAIN do { if (inside_main) abort (); } while (0)
#else
#define ABORT_INSIDE_MAIN do { } while (0)
#endif

/* These next definitions are kludges.  When GCC has a <stdint.h> it
   should be used.
*/
#include <limits.h>
#if INT_MAX == __LONG_LONG_MAX__
typedef int intmax_t;
#define INTMAX_MAX INT_MAX
#elif LONG_MAX == __LONG_LONG_MAX__
typedef long intmax_t;
#define INTMAX_MAX LONG_MAX
#else
typedef long long intmax_t;
#define INTMAX_MAX __LONG_LONG_MAX__
#endif

int
abs (int x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}

long
labs (long x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}

long long
llabs (long long x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}

intmax_t
imaxabs (intmax_t x)
{
  ABORT_INSIDE_MAIN;
  return x < 0 ? -x : x;
}
