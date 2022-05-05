/* PR c/104633 */
/* { dg-do compile } */
/* { dg-options "-Winfinite-recursion" } */

typedef __SIZE_TYPE__ size_t;
int memcmp (const void *, const void *, size_t);
__typeof (memcmp) __memcmp_alias __asm ("memcmp");

extern inline __attribute__((always_inline, gnu_inline)) int
memcmp (const void *p, const void *q, size_t size)	/* { dg-bogus "infinite recursion detected" } */
{
  return __memcmp_alias (p, q, size);			/* { dg-bogus "recursive call" } */
}

int
foo (const void *p, const void *q, size_t size)
{
  return memcmp (p, q, size);
}
