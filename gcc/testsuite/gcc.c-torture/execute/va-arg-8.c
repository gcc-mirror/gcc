/* Origin: Franz Sirl <Franz.Sirl-kernel@lauterbach.com> */

#include <varargs.h>
#include <limits.h>

#if __LONG_LONG_MAX__ == 9223372036854775807LL

typedef long long int INT64;

inline void
debug(i1, i2, i3, i4, i5, i6, i7, i8, i9, va_alist)
     int i1, i2, i3, i4, i5, i6, i7, i8, i9;
     va_dcl
{
  va_list ap;

  va_start (ap);

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
