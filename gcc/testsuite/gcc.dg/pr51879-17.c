/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

struct S {
  int i;
};

extern struct S foo (void);
extern int foo2 (void);

struct S s, s2;

int bar (int c) {
  int r;

  if (c)
    {
      s = foo ();
      r = foo2 ();
    }
  else
    {
      s2 = foo ();
      r = foo2 ();
    }

  return r;
}

/* { dg-final { scan-tree-dump-times "foo \\(" 2 "pre"} } */
/* { dg-final { scan-tree-dump-times "foo2 \\(" 2 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
