/* { dg-do compile } */

extern inline __attribute__ ((always_inline)) void *
memmove (void *dest, const void *src, __SIZE_TYPE__ len)
{
  return __builtin___memmove_chk (dest, src, len,
				  __builtin_object_size (dest, 0));
}

void
foo (void)
{
  char a[64], *b;
  for (;;)
    {
      memmove (a, b, 0);
      b = a;
    }
}
