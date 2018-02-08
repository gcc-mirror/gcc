/* { dg-do compile } */
/* { dg-options "-O2 -Wformat-overflow" } */

extern int sprintf (char *restrict, const char *restrict, ...)
     __attribute__ ((__nothrow__));
     extern int foo (void);

int
Fgenerate_new_buffer_name (void)
{
  char number[2];
  int i = foo ();
  if (i < 0)
    __builtin_unreachable ();
  if (i >= 2)
    __builtin_unreachable ();
  return sprintf (number, "%d", i); /* { dg-bogus "writing" } */
}
