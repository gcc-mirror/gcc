/* Copyright (C) 2003  Free Software Foundation.

   Ensure that builtin mempcpy and stpcpy perform correctly.

   Written by Jakub Jelinek, 21/05/2003.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *mempcpy (void *, const void *, size_t);
extern int memcmp (const void *, const void *, size_t);
extern int inside_main;

long buf1[64];
char *buf2 = (char *) (buf1 + 32);
long buf5[20];
char buf7[20];

void
__attribute__((noinline))
test (long *buf3, char *buf4, char *buf6, int n)
{
  int i = 0;

  /* These should probably be handled by store_by_pieces on most arches.  */
  if (mempcpy (buf1, "ABCDEFGHI", 9) != (char *) buf1 + 9
      || memcmp (buf1, "ABCDEFGHI\0", 11))
    abort ();

  if (mempcpy (buf1, "abcdefghijklmnopq", 17) != (char *) buf1 + 17
      || memcmp (buf1, "abcdefghijklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy (buf3, "ABCDEF", 6) != (char *) buf1 + 6
      || memcmp (buf1, "ABCDEFghijklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy (buf3, "a", 1) != (char *) buf1 + 1
      || memcmp (buf1, "aBCDEFghijklmnopq\0", 19))
    abort ();

  if (mempcpy ((char *) buf3 + 2, "bcd" + ++i, 2) != (char *) buf1 + 4
      || memcmp (buf1, "aBcdEFghijklmnopq\0", 19)
      || i != 1)
    abort ();

  /* These should probably be handled by move_by_pieces on most arches.  */
  if (mempcpy ((char *) buf3 + 4, buf5, 6) != (char *) buf1 + 10
      || memcmp (buf1, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy ((char *) buf1 + ++i + 8, (char *) buf5 + 1, 1)
      != (char *) buf1 + 11
      || memcmp (buf1, "aBcdRSTUVWSlmnopq\0", 19)
      || i != 2)
    abort ();

  if (mempcpy ((char *) buf3 + 14, buf6, 2) != (char *) buf1 + 16
      || memcmp (buf1, "aBcdRSTUVWSlmnrsq\0", 19))
    abort ();

  if (mempcpy (buf3, buf5, 8) != (char *) buf1 + 8
      || memcmp (buf1, "RSTUVWXYVWSlmnrsq\0", 19))
    abort ();

  if (mempcpy (buf3, buf5, 17) != (char *) buf1 + 17
      || memcmp (buf1, "RSTUVWXYZ01234567\0", 19))
    abort ();

  __builtin_memcpy (buf3, "aBcdEFghijklmnopq\0", 19);

  /* These should be handled either by movmemendM or mempcpy
     call.  */
  if (mempcpy ((char *) buf3 + 4, buf5, n + 6) != (char *) buf1 + 10
      || memcmp (buf1, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy ((char *) buf1 + ++i + 8, (char *) buf5 + 1, n + 1)
      != (char *) buf1 + 12
      || memcmp (buf1, "aBcdRSTUVWkSmnopq\0", 19)
      || i != 3)
    abort ();

  if (mempcpy ((char *) buf3 + 14, buf6, n + 2) != (char *) buf1 + 16
      || memcmp (buf1, "aBcdRSTUVWkSmnrsq\0", 19))
    abort ();

  i = 1;

  /* These might be handled by store_by_pieces.  */
  if (mempcpy (buf2, "ABCDEFGHI", 9) != buf2 + 9
      || memcmp (buf2, "ABCDEFGHI\0", 11))
    abort ();

  if (mempcpy (buf2, "abcdefghijklmnopq", 17) != buf2 + 17
      || memcmp (buf2, "abcdefghijklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy (buf4, "ABCDEF", 6) != buf2 + 6
      || memcmp (buf2, "ABCDEFghijklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy (buf4, "a", 1) != buf2 + 1
      || memcmp (buf2, "aBCDEFghijklmnopq\0", 19))
    abort ();

  if (mempcpy (buf4 + 2, "bcd" + i++, 2) != buf2 + 4
      || memcmp (buf2, "aBcdEFghijklmnopq\0", 19)
      || i != 2)
    abort ();

  /* These might be handled by move_by_pieces.  */
  if (mempcpy (buf4 + 4, buf7, 6) != buf2 + 10
      || memcmp (buf2, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy (buf2 + i++ + 8, buf7 + 1, 1)
      != buf2 + 11
      || memcmp (buf2, "aBcdRSTUVWSlmnopq\0", 19)
      || i != 3)
    abort ();

  if (mempcpy (buf4 + 14, buf6, 2) != buf2 + 16
      || memcmp (buf2, "aBcdRSTUVWSlmnrsq\0", 19))
    abort ();

  __builtin_memcpy (buf4, "aBcdEFghijklmnopq\0", 19);

  /* These should be handled either by movmemendM or mempcpy
     call.  */
  if (mempcpy (buf4 + 4, buf7, n + 6) != buf2 + 10
      || memcmp (buf2, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  if (__builtin_mempcpy (buf2 + i++ + 8, buf7 + 1, n + 1)
      != buf2 + 12
      || memcmp (buf2, "aBcdRSTUVWkSmnopq\0", 19)
      || i != 4)
    abort ();

  if (mempcpy (buf4 + 14, buf6, n + 2) != buf2 + 16
      || memcmp (buf2, "aBcdRSTUVWkSmnrsq\0", 19))
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
