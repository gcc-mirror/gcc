/* Copyright (C) 2003  Free Software Foundation.

   Ensure that builtin stpcpy performs correctly.

   Written by Jakub Jelinek, 21/05/2003.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern int memcmp (const void *, const void *, size_t);
extern char *stpcpy (char *, const char *);
extern int inside_main;

long buf1[64];
char *buf2 = (char *) (buf1 + 32);
long buf5[20];
char buf7[20];

void
__attribute__((noinline))
test (long *buf3, char *buf4, char *buf6, int n)
{
  int i = 4;

  if (stpcpy ((char *) buf3, "abcdefghijklmnop") != (char *) buf1 + 16
      || memcmp (buf1, "abcdefghijklmnop", 17))
    abort ();

  if (__builtin_stpcpy ((char *) buf3, "ABCDEFG") != (char *) buf1 + 7
      || memcmp (buf1, "ABCDEFG\0ijklmnop", 17))
    abort ();

  if (stpcpy ((char *) buf3 + i++, "x") != (char *) buf1 + 5
      || memcmp (buf1, "ABCDx\0G\0ijklmnop", 17))
    abort ();
}

void
main_test (void)
{
  /* All these tests are allowed to call mempcpy/stpcpy.  */
  inside_main = 0;
  __builtin_memcpy (buf5, "RSTUVWXYZ0123456789", 20);
  __builtin_memcpy (buf7, "RSTUVWXYZ0123456789", 20);
  test (buf1, buf2, "rstuvwxyz", 0);
}
