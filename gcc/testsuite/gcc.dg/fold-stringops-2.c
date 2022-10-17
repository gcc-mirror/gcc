/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

#define bos(__d) __builtin_object_size ((__d), 0)

char *
safe1 (const char *src, int cond, size_t len)
{
  char *dst;

  if (cond)
    dst = __builtin_malloc (1024);
  else
    dst = __builtin_malloc (2048);

  len = len > 2048 ? 2048 : len;

  return __builtin___memcpy_chk (dst, src, len, bos (dst));
}

char *
safe2 (const char *src, int cond, unsigned char len)
{
  char *dst;

  if (cond)
    dst = __builtin_malloc (1024);
  else
    dst = __builtin_malloc (2048);

  return __builtin___strncpy_chk (dst, src, len, bos (dst));
}

int
safe3 (const char *src, int cond, unsigned char len)
{
  char *dst;

  if (cond)
    dst = __builtin_malloc (1024);
  else
    dst = __builtin_malloc (2048);

  return __builtin___snprintf_chk (dst, len, 0, bos (dst), "%s", src);
}

char dst[1024];

void
safe4 (size_t len)
{
  len = len > sizeof (dst) - 1 ? sizeof (dst) - 1 : len;
  len = len < sizeof (dst) / 2 ? sizeof (dst) / 2 : len;

  __builtin_strncat (dst, "hello", len);
}

/* { dg-final { scan-assembler-not "__memcpy_chk" } } */
/* { dg-final { scan-assembler-not "__strncpy_chk" } } */
/* { dg-final { scan-assembler-not "__snprintf_chk" } } */
/* { dg-final { scan-assembler-not "strncat" } } */
