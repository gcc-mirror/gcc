/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  }  */

int some_f1 (int);
int some_f2 (int);
int some_f3 (int);

void remove_this_call ();

int g;

static int __attribute__((noinline))
bar (int p)
{
  if (p)
    remove_this_call ();
  return g++;
}

static int __attribute__((noinline))
foo (int (*f)(int))
{
  return bar (f == (void *)0);
}

int
baz1 (void)
{
  int (*f)(int);
  if (g)
    f = some_f1;
  else
    f = some_f2;
  return foo (f);
}

int
baz2 (void)
{
  int (*f)(int);
  if (g)
    f = some_f2;
  else
    f = some_f3;
  return foo (f);
}

/* { dg-final { scan-tree-dump-not "remove_this_call"  "optimized"  } } */
