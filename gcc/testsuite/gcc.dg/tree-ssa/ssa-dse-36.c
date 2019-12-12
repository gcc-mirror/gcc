/* { dg-options "-O2 -fdump-tree-dse-details -fno-tree-fre" } */
#include <string.h>
#include <stdlib.h>

struct X
{
  char mem0[10];
  char mem1[10];
};


void blah (struct X);


void
foo1()
{
  struct X x = { };
  memset (x.mem1, 0, sizeof x.mem1);
  blah (x);
}

void
foo2()
{
  struct X x = { };
  x.mem1[5] = 0;
  blah (x);
}

void
bar1 ()
{
  struct X x;
  memset (&x, 0, sizeof x);
  memset (&x.mem1, 0, sizeof x.mem1);
  blah (x);
}
void
bar2 ()
{
  struct X x;
  memset (&x, 0, sizeof x);
  x.mem1[5] = 0;
  blah (x);
}

void
baz1 ()
{
  struct X *x = calloc (sizeof (struct X), 1);
  memset (&x->mem1, 0, sizeof x->mem1);
  blah (*x);
}

void
baz2 ()
{
  struct X *x = calloc (sizeof (struct X), 1);
  x->mem1[5] = 0;
  blah (*x);
}
/* { dg-final { scan-tree-dump-times "Deleted redundant call" 3 "dse1" } } */
/* { dg-final { scan-tree-dump-times "Deleted redundant store" 3 "dse1" } } */

