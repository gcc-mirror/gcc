#include <varargs.h>

typedef int TYPE;

void vafunction (dummy, va_alist)
  char *dummy;
  va_dcl
{
  va_list ap;

  va_start(ap);
  if (va_arg (ap, TYPE) != 1)
    abort();
  if (va_arg (ap, TYPE) != 2)
    abort();
  if (va_arg (ap, TYPE) != 3)
    abort();
  if (va_arg (ap, TYPE) != 4)
    abort();
  if (va_arg (ap, TYPE) != 5)
    abort();
  if (va_arg (ap, TYPE) != 6)
    abort();
  if (va_arg (ap, TYPE) != 7)
    abort();
  if (va_arg (ap, TYPE) != 8)
    abort();
  if (va_arg (ap, TYPE) != 9)
    abort();
  va_end(ap);
}


int main (void)
{
  vafunction( "", 1, 2, 3, 4, 5, 6, 7, 8, 9 );
  exit(0);
  return 0;
}
