/* Copyright (C) 2000  Free Software Foundation.

   Ensure builtin strlen, strcmp, strchr, strrchr and strncpy
   perform correctly.

   Written by Jakub Jelinek, 11/7/2000.  */

extern void abort (void);
extern __SIZE_TYPE__ strlen (const char *);
extern int strcmp (const char *, const char *);
extern char *strchr (const char *, int);
extern char *strrchr (const char *, int);
extern char *strncpy (char *, const char *, __SIZE_TYPE__);
extern void *memset (void *, int, __SIZE_TYPE__);
extern int memcmp (const void *, const void *, __SIZE_TYPE__);

int x = 6;
int y = 1;
char *bar = "hi world";
char buf [64];

int main()
{
  const char *const foo = "hello world";
  char dst [64];

  if (strlen (bar) != 8)
    abort ();
  if (strlen (bar + (++x & 2)) != 6)
    abort ();
  if (x != 7)
    abort ();
  if (strlen (foo + (x++, 6)) != 5)
    abort ();
  if (x != 8)
    abort ();
  if (strlen (foo + (++x & 1)) != 10)
    abort ();
  if (x != 9)
    abort ();
  if (strcmp (foo + (x -= 6), "lo world"))
    abort ();
  if (x != 3)
    abort ();
  if (strcmp (foo, bar) >= 0)
    abort ();
  if (strcmp (foo, bar + (x++ & 1)) >= 0)
    abort ();
  if (x != 4)
    abort ();
  if (strchr (foo + (x++ & 7), 'l') != foo + 9)
    abort ();
  if (x != 5)
    abort ();
  if (strchr (bar, 'o') != bar + 4)
    abort ();
  if (strchr (bar, '\0')  != bar + 8)
    abort ();
  if (strrchr (bar, 'x'))
    abort ();
  if (strrchr (bar, 'o') != bar + 4)
    abort ();
  if (strcmp (foo + (x++ & 1), "ello world" + (--y & 1)))
    abort ();
  if (x != 6 || y != 0)
    abort ();
  dst[5] = ' ';
  dst[6] = '\0';
  x = 5;
  y = 1;
  if (strncpy (dst + 1, foo + (x++ & 3), 4) != dst + 1
      || x != 6
      || strcmp (dst + 1, "ello "))
    abort ();
  memset (dst, ' ', sizeof dst);
  if (strncpy (dst + (++x & 1), (y++ & 3) + "foo", 10) != dst + 1
      || x != 7
      || y != 2
      || memcmp (dst, " oo\0\0\0\0\0\0\0\0 ", 12))
    abort ();
  memset (dst, ' ', sizeof dst);
  if (strncpy (dst, "hello", 8) != dst || memcmp (dst, "hello\0\0\0 ", 9))
    abort ();
  x = '!';
  memset (buf, ' ', sizeof buf);
  if (memset (buf, x++, ++y) != buf
      || x != '!' + 1
      || y != 3
      || memcmp (buf, "!!!", 3))
    abort ();
  if (memset (buf + y++, '-', 8) != buf + 3
      || y != 4
      || memcmp (buf, "!!!--------", 11))
    abort ();
  x = 10;
  if (memset (buf + ++x, 0, y++) != buf + 11
      || x != 11
      || y != 5
      || memcmp (buf + 8, "---\0\0\0", 7))
    abort ();
  if (memset (buf + (x += 4), 0, 6) != buf + 15
      || x != 15
      || memcmp (buf + 10, "-\0\0\0\0\0\0\0\0\0", 11))
    abort ();

  return 0;
}
