/* PR c/119582 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=address,pointer-subtract,pointer-compare" } */

const char v;
typedef __PTRDIFF_TYPE__ ptrdiff_t;
char a;
const ptrdiff_t p = &a + 1 - &a;
const int q = (&a + 1) != &a;

ptrdiff_t
foo (void)
{
  char b;
  return &b + (v != '\n') - &b;
}

int
bar (void)
{
  char b;
  return (&b + (v != '\n')) != &b;
}
