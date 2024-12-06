// { dg-do run }
// { dg-options "-Wunused-value" }

#include <stdarg.h>

const unsigned char a[] = {
  #embed __FILE__ limit (128)
};

int
foo (int x, ...)
{
  if (x != 42)
    return 2;
  va_list ap;
  va_start (ap, x);
  for (int i = 0; i < 128; ++i)
    if (va_arg (ap, int) != a[i])
      {
	va_end (ap);
	return 1;
      }
  va_end (ap);
  return 0;
}

int b, c;

int
main ()
{
  if (foo (42,
#embed __FILE__ limit (128)
      ))
    __builtin_abort ();
  b = (
#embed __FILE__ limit (128) prefix (c = 2 * ) suffix ( + 6)	// { dg-warning "right operand of comma operator has no effect" }
  );
  if (b != a[127] + 6 || c != 2 * a[0])
    __builtin_abort ();
}
