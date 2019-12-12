/* PR tree-optimization/89500 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;
extern size_t strlen (const char *);
extern size_t strnlen (const char *, size_t);
extern void bar (char *);

void
foo (int *a)
{
  char c[64];
  bar (c);
  a[0] = strlen (c);
  a[1] = strnlen (c, 0);
}
