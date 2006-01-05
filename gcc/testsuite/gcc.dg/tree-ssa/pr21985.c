/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

struct
{
  struct
    {
      int a;
      int b;
    } a[100];
} a;

typedef __SIZE_TYPE__ size_t;
void foo(size_t);
size_t *bar (void);

int
main (void)
{
  size_t *b = bar ();

  /* This should be folded.  */
  foo (&a.a[50].a - &a.a[33].b);
  foo ((size_t) &a.a[50].b - (size_t) &a);

  /* And this should not.  */
  foo ((size_t) &b - (size_t) b);
  return 0;
}

/* Two of the calls to foo should be folded to just foo(constant).  */

/* { dg-final { scan-tree-dump-times "foo \\(\[0-9\]*\\)" 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
