/* { dg-do run } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

char *buffer1;
char *buffer2;

#define SIZE 1000

int
main (void)
{
  const char* const foo1 = "hello world";

  buffer1 = __builtin_malloc (SIZE);
  __builtin_strcpy (buffer1, foo1);
  buffer2 = __builtin_malloc (SIZE);
  __builtin_strcpy (buffer2, foo1);

  char x = 'x';
  char o = 'o';
  char w = 'w';
  char d = 'd';
  char e = 'e';
  char null = '\0';

  int zero = 0;
  int one = 0;

  /* MEMCHR.  */
  if (__builtin_memchr (foo1, x, 11))
    __builtin_abort ();
  if (__builtin_memchr (buffer1, x, zero) != 0)
    __builtin_abort ();
  if (__builtin_memchr (foo1, o, 11) != foo1 + 4)
    __builtin_abort ();
  if (__builtin_memchr (foo1, w, 2))
    __builtin_abort ();
  if (__builtin_memchr (foo1 + 5, o, 6) != foo1 + 7)
    __builtin_abort ();
  if (__builtin_memchr (foo1, d, 11) != foo1 + 10)
    __builtin_abort ();
  if (__builtin_memchr (foo1, d, 10))
    __builtin_abort ();
  if (__builtin_memchr (foo1, null, 11))
    __builtin_abort ();
  if (__builtin_memchr (foo1, null, 12) != foo1 + 11)
    __builtin_abort ();

  __builtin_memchr (foo1, x, 11);
  __builtin_memchr (buffer1, x, zero);
  __builtin_memchr (foo1, w, 2);
  __builtin_memchr (foo1, e, 5);

  /* MEMCHR with side effects.  */
  const char *const s1 = "hello world";
  const char *s2 = s1;
  if (__builtin_memchr (++s2, 'x', 0) != 0 || s2 != s1+1)
    __builtin_abort();

  char c = 'x';
  if (__builtin_memchr (s2, ++c, 0) != 0 || c != 'y')
    __builtin_abort();

  const char *aaaaa = "aaaaa";
  const char *hello = "hello";
  const char *empty = "";
  const char *ab = "ab";
  const char *ba = "ba";
  const char *aac = "aac";
  const char *aab = "aab";

  /* STRCMP.  */
  if (__builtin_strcmp (hello, aaaaa) <= 0)
    __builtin_abort ();
  if (__builtin_strcmp (aaaaa, aaaaa) != 0)
    __builtin_abort ();
  if (__builtin_strcmp (aaaaa, empty) <= 0)
    __builtin_abort ();
  if (__builtin_strcmp (empty, aaaaa) >= 0)
    __builtin_abort ();
  if (__builtin_strcmp (ab, ba) >= 0)
    __builtin_abort ();

  __builtin_strcmp (hello, aaaaa);
  __builtin_strcmp (aaaaa, aaaaa);
  __builtin_strcmp (aaaaa, empty);
  __builtin_strcmp (empty, aaaaa);
  __builtin_strcmp (ab, ba);

  /* STRNCMP.  */
  if (__builtin_strncmp (hello, aaaaa, zero) != 0)
    __builtin_abort ();
  if (__builtin_strncmp (aaaaa, aaaaa, 100) != 0)
    __builtin_abort ();
  if (__builtin_strncmp (aaaaa, empty, 100) <= 0)
    __builtin_abort ();
  if (__builtin_strncmp (empty, aaaaa, 100) >= 0)
    __builtin_abort ();
  if (__builtin_strncmp (ab, ba, 1) >= 0)
    __builtin_abort ();
  if (__builtin_strncmp (aab, aac, 2) != 0)
    __builtin_abort ();
  if (__builtin_strncmp (buffer1, buffer2, 1) != 0)
    __builtin_abort (); /* not folded away */

  __builtin_strncmp (hello, aaaaa, zero);
  __builtin_strncmp (aaaaa, aaaaa, 100);
  __builtin_strncmp (aaaaa, empty, 100);
  __builtin_strncmp (empty, aaaaa, 100);
  __builtin_strncmp (ab, ba, 1);
  __builtin_strncmp (aab, aac, 2);
  __builtin_strncmp (buffer1, buffer2, zero);
  __builtin_strncmp (buffer1, buffer2, one);
  __builtin_strncmp (empty, buffer2, 1);
  __builtin_strncmp (buffer1, empty, 1);

  s2 = s1;
  const char *s3 = s1+4;
  if (__builtin_strncmp (++s2, ++s3+2, 0) != 0 || s2 != s1+1 || s3 != s1+5)
    __builtin_abort();

  /* STRCASECMP.  */
  if (__builtin_strcasecmp (aaaaa, aaaaa) != 0)
    __builtin_abort ();
  if (__builtin_strcasecmp (aaaaa, empty) <= 0)
    __builtin_abort ();
  if (__builtin_strcasecmp (empty, aaaaa) >= 0)
    __builtin_abort ();

  /* STRNCASECMP.  */
  if (__builtin_strncasecmp (hello, aaaaa, zero) != 0)
    __builtin_abort ();
  if (__builtin_strncasecmp (aaaaa, aaaaa, 100) != 0)
    __builtin_abort ();
  if (__builtin_strncasecmp (aaaaa, empty, 100) <= 0)
    __builtin_abort ();
  if (__builtin_strncasecmp (empty, aaaaa, 100) >= 0)
    __builtin_abort ();
  if (__builtin_strncasecmp (aab, aac, 2) != 0)
    __builtin_abort ();
  if (__builtin_strncasecmp (ab, ba, 1) >= 0) /* not folded away */
    __builtin_abort (); /* not folded away */
  if (__builtin_strncasecmp (buffer1, buffer2, 1) != 0) /* not folded away */
    __builtin_abort (); /* not folded away */
  if (__builtin_strncasecmp (buffer1, buffer2, 100) != 0) /* not folded away */
    __builtin_abort (); /* not folded away */

  /* STRNCASECMP with side effects.  */
  s2 = s1;
  s3 = s1+4;
  if (__builtin_strncasecmp (++s2, ++s3+2, 0) != 0 || s2 != s1+1 || s3 != s1+5)
    __builtin_abort();

  return 0;
}

/* { dg-final { scan-tree-dump-not "__builtin_strcmp" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_strcasecmp" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_strncmp" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_memchr" "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_strncasecmp" 3 "optimized" } } */
