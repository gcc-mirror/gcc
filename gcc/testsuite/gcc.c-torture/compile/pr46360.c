/* PR middle-end/46360 */

__attribute__((gnu_inline, always_inline)) extern inline char *
strncpy (char *dest, const char *src, __SIZE_TYPE__ len)
{
  return __builtin_strncpy (dest, src, len);
}

void
foo (char *s)
{
  strncpy (s, "", 0);
}
