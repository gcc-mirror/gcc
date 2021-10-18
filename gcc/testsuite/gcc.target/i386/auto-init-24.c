/* Verify the auto initialization of nested VLA.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-rtl-expand" } */

void g(void *);

void foo(int a)
{
  int x[a][a];
  g(x);
}

/* { dg-final { scan-rtl-dump "__builtin_memset" "expand" } } */
