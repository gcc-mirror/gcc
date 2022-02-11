/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

char dst[1024];

void
safe1 (size_t len)
{
  len = len > sizeof (dst) ? sizeof (dst) : len;
  len = len < sizeof (dst) / 2 ? sizeof (dst) / 2 : len;

  __builtin_snprintf (dst, len, "hello");
  __builtin_snprintf (dst + 5, len, "%s", " world");
}

/* { dg-final { scan-assembler-not "snprintf" } } */
