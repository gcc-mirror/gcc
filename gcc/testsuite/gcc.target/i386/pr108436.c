/* { dg-do compile } */
/* { dg-options "-mprefetchi" } */

int
foo (int a)
{
  return a + 1;
}

void
bad (int *p)
{
  __builtin_ia32_prefetch (p, 0, 4, 0);   /* { dg-warning "invalid third argument to '__builtin_ia32_prefetch'; using zero" } */
  __builtin_ia32_prefetch (foo, 0, 4, 1);   /* { dg-error "invalid third argument" } */
}
