#include <varargs.h>

typedef double TYPE;

void vafunction (dummy1, dummy2, va_alist)
  TYPE dummy1, dummy2;
  va_dcl
{
  va_list ap;

  va_start(ap);
  if (dummy1 != 888.)
    abort();
  if (dummy2 != 999.)
    abort();
  if (va_arg (ap, TYPE) != 1.)
    abort();
  if (va_arg (ap, TYPE) != 2.)
    abort();
  if (va_arg (ap, TYPE) != 3.)
    abort();
  if (va_arg (ap, TYPE) != 4.)
    abort();
  if (va_arg (ap, TYPE) != 5.)
    abort();
  if (va_arg (ap, TYPE) != 6.)
    abort();
  if (va_arg (ap, TYPE) != 7.)
    abort();
  if (va_arg (ap, TYPE) != 8.)
    abort();
  if (va_arg (ap, TYPE) != 9.)
    abort();
  va_end(ap);
}


int main (void)
{
  vafunction( 888., 999., 1., 2., 3., 4., 5., 6., 7., 8., 9. );
  exit(0);
  return 0;
}
