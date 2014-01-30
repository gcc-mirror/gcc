/* PR c/58346 */
/* { dg-do compile } */

struct U {
#ifdef __cplusplus
  char a[0];
#endif
};
static struct U b[6];
static struct U *u1, *u2;

int
foo (struct U *p, struct U *q)
{
  return q - p; /* { dg-error "arithmetic on pointer to an empty aggregate" } */
}

void
bar (void)
{
  __PTRDIFF_TYPE__ d = u1 - u2; /* { dg-error "arithmetic on pointer to an empty aggregate" } */
  __asm volatile ("" : "+g" (d));
  foo (&b[0], &b[4]);
}
