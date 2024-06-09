/* PR target/44575 */

#include <stdarg.h>

void abort (void);

int fails = 0;
struct S { float a[3]; };
struct S a[5];

void
check (int z, ...)
{
  struct S arg, *p;
  va_list ap;
  int j = 0, k = 0;
  int i;
  va_start (ap, z);
  for (i = 2; i < 4; ++i)
    {
      p = 0;
      j++;
      k += 2;
      switch ((z << 4) | i)
	{
	case 0x12:
	case 0x13:
	  p = &a[2];
	  arg = va_arg (ap, struct S);
	  break;
	default:
	  ++fails;
	  break;
	}
      if (p && p->a[2] != arg.a[2])
	++fails;
      if (fails)
	break;
    }
  va_end (ap);
}

int
main ()
{
  a[2].a[2] = -49026;
  check (1, a[2], a[2]);
  if (fails)
    abort ();
  return 0;
}
