/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
struct foo
{
  int val;
} *fooptr;
struct bar
{
  struct foo foo;
  int val2;
} *barptr;
int
test ()
{
  struct foo foo = { 0 };
  barptr->val2 = 123;
  *fooptr = foo;
  return barptr->val2;
}

/* { dg-final { scan-tree-dump-times "return 123" 1 "fre1"} } */
