/* A testcase for PR 40744.  We do not want to create replacements for object
   that are dead or have only a single use, whenever it can be avoided
   simply. */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-esra-details" } */

struct X { int i; int j; };

void foo(void)
{
  struct X x;
  x.i = 1;
  x.j = 2;
}


int bar(struct X x)
{
  return x.i;
}


extern void do_something (struct X);

void bar2(int i, int j)
{
  struct X x;

  x.i = i;
  x.j = j;
  do_something (x);
}

/* { dg-final { scan-tree-dump-times "Created a replacement" 0 "esra"} } */
