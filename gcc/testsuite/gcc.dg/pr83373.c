/* PR middle-end/83373 - False positive reported by -Wstringop-overflow
   { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow" }  */

typedef __SIZE_TYPE__ size_t;

char buf[100];

void get_data (char*);

__attribute__ ((nonnull(1, 2)))
inline char* my_strcpy (char* dst, const char* src, size_t size)
{
  size_t len = __builtin_strlen (src);
  if (len < size)
    __builtin_memcpy (dst, src, len + 1);
  else
    {
      __builtin_memcpy (dst, src, size - 1); /* { dg-bogus "\\\[-Wstringop-oveflow]" } */
      dst[size - 1] = '\0';
    }

  return dst;
}

void test(void)
{
  char data[20] = "12345";

  get_data (data);

  my_strcpy (buf, data, sizeof buf);
}
