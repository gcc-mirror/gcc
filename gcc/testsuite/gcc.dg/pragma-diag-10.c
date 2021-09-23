/* PR middle-end/98512 - #pragma GCC diagnostic ignored ineffective
   in conjunction with alias attribute
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void *
__rawmemchr_ppc (const void *s, int c)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
#pragma GCC diagnostic ignored "-Wstringop-overread"
  if (c != 0)
    return __builtin_memchr (s, c, (unsigned long)-1);    // { dg-bogus "specified bound \\d+ exceeds maximum object size" }
#pragma GCC diagnostic pop

  return (char *)s + __builtin_strlen (s);
}

extern __typeof (__rawmemchr_ppc) __EI___rawmemchr_ppc
  __attribute__((alias ("__rawmemchr_ppc")));
