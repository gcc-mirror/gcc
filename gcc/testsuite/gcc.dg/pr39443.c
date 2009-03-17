/* PR middle-end/39443 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "memcmp" } } */

typedef __SIZE_TYPE__ size_t;

extern int memcmp (const void *s1, const void *s2, size_t n)
  __attribute__ ((__nothrow__, __pure__));
extern __typeof (memcmp) memcmp __asm__ ("memory_compare");

int
test (char *s, char *t, int cnt)
{
  if (__builtin_expect (cnt, 0))
    return memcmp (s, t, cnt);
  return 0;
}
