/* PR target/61060 */
/* { dg-do compile } */
/* { dg-options "-O0 -ftree-ter" } */

typedef __SIZE_TYPE__ size_t;

extern inline __attribute__ ((gnu_inline, always_inline, artificial))
void *memset (void *dest, int ch, size_t len)
{
  return __builtin_memset (dest, ch, len);
}

char buf[10];

void
foo (void)
{
  memset (buf, sizeof (buf), 0);
}
