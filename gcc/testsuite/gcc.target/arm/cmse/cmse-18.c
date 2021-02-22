/* { dg-do compile } */
/* { dg-options "-mcmse -fdump-rtl-final-slim" } */

typedef void (*f)(int) __attribute__((cmse_nonsecure_call));

void bar(f func, int a)
{
  func(a);
}

/* { dg-final { scan-rtl-dump "call unspec\\\[\\\[r4:SI\\\]\\\]" "final" } } */
