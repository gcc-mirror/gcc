/* PR c/104633 */
/* { dg-do compile } */
/* { dg-options "-Winfinite-recursion" } */

typedef __SIZE_TYPE__ size_t;
int memcmp (const void *, const void *, size_t);

extern inline __attribute__((always_inline, gnu_inline)) int
memcmp (const void *p, const void *q, size_t size)	/* { dg-warning "infinite recursion detected" } */
{							/* { dg-error "inlining failed in call to" "" { target *-*-* } .-1 } */
  return memcmp (p, q, size);				/* { dg-message "recursive call" } */
}							/* { dg-message "called from here" "" { target *-*-* } .-1 } */

int
foo (const void *p, const void *q, size_t size)
{
  return memcmp (p, q, size);
}
