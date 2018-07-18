/* Test built-in string functions with large sizes.  PR 78460.  */

typedef __SIZE_TYPE__ size_t;

#define SIZE1 ((size_t) -1)
#define SIZE2 (SIZE1 >> 1)
#define SIZE3 ((unsigned int) -1)
#define SIZE4 (SIZE3 >> 1)

volatile int v1, v2, v3, v4;
void *volatile vp1, *volatile vp2, *volatile vp3, *volatile vp4;

void
test_memchr (const void *a, int b)
{
  vp1 = __builtin_memchr (a, b, SIZE1);
  vp2 = __builtin_memchr (a, b, SIZE2);
  vp3 = __builtin_memchr (a, b, SIZE3);
  vp4 = __builtin_memchr (a, b, SIZE4);
}

void
test_memcmp (const void *a, const void *b)
{
  v1 = __builtin_memcmp (a, b, SIZE1);
  v2 = __builtin_memcmp (a, b, SIZE2);
  v3 = __builtin_memcmp (a, b, SIZE3);
  v4 = __builtin_memcmp (a, b, SIZE4);
}

void
test_memcpy (void *a, const void *b)
{
  vp1 = __builtin_memcpy (a, b, SIZE1);
  vp2 = __builtin_memcpy (a, b, SIZE2);
  vp3 = __builtin_memcpy (a, b, SIZE3);
  vp4 = __builtin_memcpy (a, b, SIZE4);
}

void
test_memmove (void *a, const void *b)
{
  vp1 = __builtin_memmove (a, b, SIZE1);
  vp2 = __builtin_memmove (a, b, SIZE2);
  vp3 = __builtin_memmove (a, b, SIZE3);
  vp4 = __builtin_memmove (a, b, SIZE4);
}

void
test_mempcpy (void *a, const void *b)
{
  vp1 = __builtin_mempcpy (a, b, SIZE1);
  vp2 = __builtin_mempcpy (a, b, SIZE2);
  vp3 = __builtin_mempcpy (a, b, SIZE3);
  vp4 = __builtin_mempcpy (a, b, SIZE4);
}

void
test_memset (void *a, int b)
{
  vp1 = __builtin_memset (a, b, SIZE1);
  vp2 = __builtin_memset (a, b, SIZE2);
  vp3 = __builtin_memset (a, b, SIZE3);
  vp4 = __builtin_memset (a, b, SIZE4);
}

void
test_stpncpy (char *a, const char *b)
{
  vp1 = __builtin_stpncpy (a, b, SIZE1);
  vp2 = __builtin_stpncpy (a, b, SIZE2);
  vp3 = __builtin_stpncpy (a, b, SIZE3);
  vp4 = __builtin_stpncpy (a, b, SIZE4);
}

void
test_strndup (const char *a)
{
  vp1 = __builtin_strndup (a, SIZE1);
  vp2 = __builtin_strndup (a, SIZE2);
  vp3 = __builtin_strndup (a, SIZE3);
  vp4 = __builtin_strndup (a, SIZE4);
}

void
test_strncasecmp (const char *a, const char *b)
{
  v1 = __builtin_strncasecmp (a, b, SIZE1);
  v2 = __builtin_strncasecmp (a, b, SIZE2);
  v3 = __builtin_strncasecmp (a, b, SIZE3);
  v4 = __builtin_strncasecmp (a, b, SIZE4);
}

void
test_strncat (char *a, const char *b)
{
  vp1 = __builtin_strncat (a, b, SIZE1);
  vp2 = __builtin_strncat (a, b, SIZE2);
  vp3 = __builtin_strncat (a, b, SIZE3);
  vp4 = __builtin_strncat (a, b, SIZE4);
}

void
test_strncmp (const char *a, const char *b)
{
  v1 = __builtin_strncmp (a, b, SIZE1);
  v2 = __builtin_strncmp (a, b, SIZE2);
  v3 = __builtin_strncmp (a, b, SIZE3);
  v4 = __builtin_strncmp (a, b, SIZE4);
}

void
test_strncpy (char *a, const char *b)
{
  vp1 = __builtin_strncpy (a, b, SIZE1);
  vp2 = __builtin_strncpy (a, b, SIZE2);
  vp3 = __builtin_strncpy (a, b, SIZE3);
  vp4 = __builtin_strncpy (a, b, SIZE4);
}
