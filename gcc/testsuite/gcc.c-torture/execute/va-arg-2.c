/* The purpose of this test is to catch edge cases when arguments are passed
   in regs and on the stack.  We test 16 cases, trying to catch multiple
   targets (some use 3 regs for argument passing, some use 12, etc.).
   We test both the arguments and the `lastarg' (the argument to va_start).  */

#include <stdarg.h>

extern __SIZE_TYPE__ strlen ();

int
to_hex (unsigned int a)
{
  static char hex[] = "0123456789abcdef";

  if (a > 15)
    abort ();
  return hex[a];
}

void
f0 (char* format, ...)
{
  va_list ap;

  va_start (ap, format);
  if (strlen (format) != 16 - 0)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f1 (int a1, char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 1)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f2 (int a1, int a2, char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 2)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f3 (int a1, int a2, int a3, char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 3)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f4 (int a1, int a2, int a3, int a4, char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 4)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f5 (int a1, int a2, int a3, int a4, int a5,
    char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 5)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f6 (int a1, int a2, int a3, int a4, int a5,
    int a6,
    char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 6)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f7 (int a1, int a2, int a3, int a4, int a5,
    int a6, int a7,
    char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 7)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f8 (int a1, int a2, int a3, int a4, int a5,
    int a6, int a7, int a8,
    char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 8)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f9 (int a1, int a2, int a3, int a4, int a5,
     int a6, int a7, int a8, int a9,
     char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 9)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f10 (int a1, int a2, int a3, int a4, int a5,
     int a6, int a7, int a8, int a9, int a10,
     char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 10)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f11 (int a1, int a2, int a3, int a4, int a5,
     int a6, int a7, int a8, int a9, int a10,
     int a11,
     char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 11)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f12 (int a1, int a2, int a3, int a4, int a5,
     int a6, int a7, int a8, int a9, int a10,
     int a11, int a12,
     char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 12)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f13 (int a1, int a2, int a3, int a4, int a5,
     int a6, int a7, int a8, int a9, int a10,
     int a11, int a12, int a13,
     char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 13)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f14 (int a1, int a2, int a3, int a4, int a5,
     int a6, int a7, int a8, int a9, int a10,
     int a11, int a12, int a13, int a14,
     char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 14)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f15 (int a1, int a2, int a3, int a4, int a5,
     int a6, int a7, int a8, int a9, int a10,
     int a11, int a12, int a13, int a14, int a15,
     char* format, ...)
{
  va_list ap;

  va_start(ap, format);
  if (strlen (format) != 16 - 15)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

main ()
{
  char *f = "0123456789abcdef";

  f0 (f+0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f1 (0, f+1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f2 (0, 1, f+2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f3 (0, 1, 2, f+3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f4 (0, 1, 2, 3, f+4, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f5 (0, 1, 2, 3, 4, f+5, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f6 (0, 1, 2, 3, 4, 5, f+6, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f7 (0, 1, 2, 3, 4, 5, 6, f+7, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  f8 (0, 1, 2, 3, 4, 5, 6, 7, f+8, 8, 9, 10, 11, 12, 13, 14, 15);
  f9 (0, 1, 2, 3, 4, 5, 6, 7, 8, f+9, 9, 10, 11, 12, 13, 14, 15);
  f10 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, f+10, 10, 11, 12, 13, 14, 15);
  f11 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, f+11, 11, 12, 13, 14, 15);
  f12 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, f+12, 12, 13, 14, 15);
  f13 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, f+13, 13, 14, 15);
  f14 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, f+14, 14, 15);
  f15 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, f+15, 15);

  exit (0);
}
