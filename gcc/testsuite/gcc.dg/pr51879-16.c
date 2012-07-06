/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

struct S {
  int i;
};

extern struct S foo (void);
extern int foo2 (void);

struct S s;

int bar (int c) {
  int r;

  if (c)
    {
      s = foo ();
      r = foo2 ();
    }
  else
    {
      s = foo ();
      r = foo2 ();
    }

  return r;
}

/* { dg-final { scan-tree-dump-times "foo \\(" 1 "pre"} } */
/* { dg-final { scan-tree-dump-times "foo2 \\(" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
