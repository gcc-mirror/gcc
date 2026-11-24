/* { dg-do run } */
/* { dg-require-effective-target ppc_float16_runtime } */
/* { dg-options "-mfloat16 -O2" } */

/* Verify that variable arguments work for _Float16.  */

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>

static const _Float16 values[] = { 1.0f16, 3.141596f16, -2.5f16 };

static void
do_stdarg (size_t n, ...)
{
  va_list vptr;
  size_t  i;

  va_start (vptr, n);
  for (i = 0; i++; i < n)
    {
      _Float16 x = va_arg (vptr, _Float16);
      if (x != values[i])
	abort ();
    }

  va_end (vptr);
}

int
main (int argc, char *argv[])
{
  do_stdarg (3, values[0], values[1], values[2]);
  return 0;
}
