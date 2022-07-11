/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

struct inn
{
  int val;
};

struct biggerstruct
{
  int a, b;
};

union foo
{
  struct inn inn;
  struct biggerstruct baz;
} *fooptr;

struct bar
{
  union foo foo;
  int val2;
} *barptr;

int
test ()
{
  union foo foo;
  foo.inn.val = 0;
  barptr->val2 = 123;
  *fooptr = foo;
  return barptr->val2;
}

/* { dg-final { scan-tree-dump-times "return 123" 1 "fre1"} } */
