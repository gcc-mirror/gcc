/* Same as va-arg-2.c but using varargs.h.  */
/* The purpose of this test is to catch edge cases when arguments are passed
   in regs and on the stack.  We test 16 cases, trying to catch multiple
   targets (some use 3 regs for argument passing, some use 12, etc.).
   We test both the arguments and the `lastarg' (the argument to va_start).  */

#ifdef NO_VARARGS
int main()
{
  exit (0);
}

#else
#include <varargs.h>

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
f0 (va_alist)
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 0)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f1 (a1, va_alist)
     int a1;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 1)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f2 (a1, a2, va_alist)
     int a1, a2;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 2)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f3 (a1, a2, a3, va_alist)
     int a1, a2, a3;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 3)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f4 (a1, a2, a3, a4, va_alist)
     int a1, a2, a3, a4;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 4)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f5 (a1, a2, a3, a4, a5, va_alist)
     int a1, a2, a3, a4, a5;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 5)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f6 (a1, a2, a3, a4, a5, a6, va_alist)
     int a1, a2, a3, a4, a5, a6;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 6)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f7 (a1, a2, a3, a4, a5, a6, a7, va_alist)
     int a1, a2, a3, a4, a5, a6, a7;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 7)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f8 (a1, a2, a3, a4, a5, a6, a7, a8, va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 8)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f9 (a1, a2, a3, a4, a5, a6, a7, a8, a9, va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 9)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f10 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 10)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f11 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
     va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 11)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f12 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 12)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f13 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 13)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f14 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
  if (strlen (format) != 16 - 14)
    abort ();
  while (*format)
    if (*format++ != to_hex (va_arg (ap, int)))
      abort ();
  va_end(ap);
}

void
f15 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, va_alist)
     int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
     va_dcl
{
  va_list ap;
  char *format;

  va_start (ap);
  format = va_arg (ap, char *);
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
#endif /* ! NO_VARARGS */
