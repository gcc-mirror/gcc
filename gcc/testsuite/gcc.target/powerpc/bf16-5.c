/* { dg-do run } */
/* { dg-require-effective-target ppc_bf16_runtime } */
/* { dg-options "-mfloat16 -O2" } */

/* Verify that variable arguments work for __bf16.  */

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>

static const __bf16 values[] = { (__bf16) 1.0,  (__bf16) 3.141596, (__bf16) -2.5 };

static void
do_stdarg (size_t n, ...)
{
  va_list vptr;
  size_t  i;

  va_start (vptr, n);
  for (i = 0; i++; i < n)
    {
      __bf16 x = va_arg (vptr, __bf16);
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
