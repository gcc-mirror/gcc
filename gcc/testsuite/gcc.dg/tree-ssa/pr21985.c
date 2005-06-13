/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-vars" } */

struct
{
  struct
    {
      int a;
      int b;
    } a[100];
} a;

void foo(unsigned);
unsigned *bar (void);

int
main (void)
{
  unsigned *b = bar ();

  /* This should be folded.  */
  foo (&a.a[50].a - &a.a[33].b);
  foo ((unsigned) &a.a[50].b - (unsigned) &a);

  /* And this should not.  */
  foo ((unsigned) &b - (unsigned) b);
  return 0;
}

/* Two of the calls to foo should be folded to just foo(constant).  */

/* { dg-final { scan-tree-dump-times "foo \\(\[0-9\]*\\)" 2 "vars" } } */
