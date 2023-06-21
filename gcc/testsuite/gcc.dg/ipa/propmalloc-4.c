/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-local-pure-const-details -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

void *foo(int cond1, int cond2, int cond3)
{
  void *ret;
  void *a;
  void *b;

  if (cond1)
    a = __builtin_malloc (10);
  else
    a = __builtin_malloc (20);

  if (cond2)
    b = __builtin_malloc (30);
  else
    b = __builtin_malloc (40);

  if (cond3)
    ret = a;
  else
    ret = b;

  return ret;
}

void *foo2(int cond1, int cond2, int cond3)
{
  void *ret;
  void *a;
  void *b;
  void bar(void *, void *);

  if (cond1)
    a = __builtin_malloc (10);
  else
    a = __builtin_malloc (20);

  if (cond2)
    b = __builtin_malloc (30);
  else
    b = __builtin_malloc (40);

  bar (a, b);

  if (cond3)
    ret = a;
  else
    ret = b;

  return ret;
}

/* { dg-final { scan-tree-dump "Function found to be malloc: foo" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "Function found to be malloc: foo2" "local-pure-const1" } } */
