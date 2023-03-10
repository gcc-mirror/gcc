/* { dg-do run } */
/* { dg-options "-fdump-tree-alias-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-fno-fat-lto-objects" } { "" } } */

typedef __INTPTR_TYPE__ intptr_t;

int i;
void __attribute__((noinline))
foo(long b, intptr_t q)
{
  int *y;
  int **a = &y, **x;
  int ***p;
  if (b)
    p = (int ***)q;
  else
    p = &a;
  x = *p;
  *x = &i;  /* *ANYTHING = &i has to make sure that y points to i.  */
  *y = 0;
}
extern void abort (void);
int main()
{
  i = 1;
  foo (0, 0);
  if (i != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "y.\?.._. = { i }" "alias" } } */
/* { dg-final { scan-tree-dump "y.\?.._., points-to NULL, points-to vars: { D..... }" "alias" } } */
