/* Copyright (C) 2003  Free Software Foundation.

   Test strcpy optimizations don't evaluate side-effects twice.
      
   Written by Jakub Jelinek, June 23, 2003.  */

typedef __SIZE_TYPE__ size_t;
extern char *strcpy (char *, const char *);
extern int memcmp (const void *, const void *, size_t);
extern void abort (void);
extern void exit (int);

size_t
test1 (char *s, size_t i)
{
  strcpy (s, "foobarbaz" + i++);
  return i;
}

size_t
check2 (void)
{
  static size_t r = 5;
  if (r != 5)
    abort ();
  return ++r;
}

void
test2 (char *s)
{
  strcpy (s, "foobarbaz" + check2 ());
}

int
main (void)
{
  char buf[10];
  if (test1 (buf, 7) != 8 || memcmp (buf, "az", 3))
    abort ();
  test2 (buf);
  if (memcmp (buf, "baz", 4))
    abort ();
  exit (0);
}
