/* Origin: Franz Sirl <Franz.Sirl-kernel@lauterbach.com> */

#include <stdarg.h>
#include <limits.h>

#if __LONG_LONG_MAX__ == 9223372036854775807LL

typedef long long int INT64;

inline void
debug(int i1, int i2, int i3, int i4, int i5,
      int i6, int i7, int i8, int i9, ...)
{
  va_list ap;

  va_start (ap, i9);

  if (va_arg (ap,int) != 10)
    abort ();
  if (va_arg (ap,INT64) != 0x123400005678LL)
    abort ();

  va_end (ap);
}

int
main(void)
{
  debug(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0x123400005678LL);
  exit(0);
}

#else

int
main(void)
{
  exit(0);
}

#endif /* long long 64 bits */
