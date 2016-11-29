/* { dg-do run } */
/* { dg-options "-O1 -fdump-tree-original" } */

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

  /* MEMCHR.  */
  if (__builtin_memchr ("hello world", 'x', 11))
    __builtin_abort ();
  if (__builtin_memchr ("hello world", 'x', 0) != 0)
    __builtin_abort ();
  if (__builtin_memchr ("hello world", 'w', 2))
    __builtin_abort ();
  if (__builtin_memchr ("hello world", 'd', 10))
    __builtin_abort ();
  if (__builtin_memchr ("hello world", '\0', 11))
    __builtin_abort ();

  /* STRCMP.  */
  if (__builtin_strcmp ("hello", "aaaaa") <= 0)
    __builtin_abort ();
  if (__builtin_strcmp ("aaaaa", "aaaaa") != 0)
    __builtin_abort ();
  if (__builtin_strcmp ("aaaaa", "") <= 0)
    __builtin_abort ();
  if (__builtin_strcmp ("", "aaaaa") >= 0)
    __builtin_abort ();
  if (__builtin_strcmp ("ab", "ba") >= 0)
    __builtin_abort ();

  /* STRNCMP.  */
  if (__builtin_strncmp ("hello", "aaaaa", 0) != 0)
    __builtin_abort ();
  if (__builtin_strncmp ("aaaaa", "aaaaa", 100) != 0)
    __builtin_abort ();
  if (__builtin_strncmp ("aaaaa", "", 100) <= 0)
    __builtin_abort ();
  if (__builtin_strncmp ("", "aaaaa", 100) >= 0)
    __builtin_abort ();
  if (__builtin_strncmp ("ab", "ba", 1) >= 0)
    __builtin_abort ();
  if (__builtin_strncmp ("aab", "aac", 2) != 0)
    __builtin_abort ();

  /* STRCASECMP.  */
  if (__builtin_strcasecmp ("aaaaa", "aaaaa") != 0)
    __builtin_abort ();

  /* STRNCASECMP.  */
  if (__builtin_strncasecmp ("hello", "aaaaa", 0) != 0)
    __builtin_abort ();
  if (__builtin_strncasecmp ("aaaaa", "aaaaa", 100) != 0)
    __builtin_abort ();
  if (__builtin_strncasecmp ("aab", "aac", 2) != 0)
    __builtin_abort ();

  /* MEMCMP.  */
  if (__builtin_memcmp ("aaaaa", "aaaaa", 6) != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "__builtin_strcmp" "original" } } */
/* { dg-final { scan-tree-dump-not "__builtin_strcasecmp" "original" } } */
/* { dg-final { scan-tree-dump-not "__builtin_strncmp" "original" } } */
/* { dg-final { scan-tree-dump-not "__builtin_strncasecmp" "original" } } */
/* { dg-final { scan-tree-dump-not "__builtin_memchr" "original" } } */
/* { dg-final { scan-tree-dump-not "__builtin_memcmp" "original" } } */
