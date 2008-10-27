/* PR tree-optimization/37879 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

static inline void bar (int) __attribute__ ((noreturn));
void baz () __attribute__ ((noreturn));

inline int
foo (int i)
{
  return i;
}

int i = 23;
static inline void
bar (int j)
{
  if (j)
    asm ("");
}		/* { dg-warning "does return" } */

void
baz ()
{
  int j;
  bar (foo (j = i++));
  asm ("");
}
