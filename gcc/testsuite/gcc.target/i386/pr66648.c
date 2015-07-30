/* { dg-do run } */
/* { dg-options "-O2 -mstringop-strategy=unrolled_loop -mtune=nocona" } */

#define PATTERN 0xdeadbeef
#define SIZE    32

struct S { int i; char str[SIZE]; int j; };

void __attribute__((noclone, noinline))
my_memcpy (char *, const char *, unsigned int);

void
my_memcpy (char *dst, const char *src, unsigned int len)
{
  if (len < 8)
    __builtin_abort ();

  __builtin_memcpy (dst, src, len);
}

int
main (void)
{
  const char str[SIZE]= "1234567890123456789012345678901";
  struct S *s = __builtin_malloc (sizeof (struct S));

  s->j = PATTERN;
  my_memcpy (s->str, str, SIZE);
  if (s->j != PATTERN)
    __builtin_abort ();

  return 0;
}
