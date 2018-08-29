// PR c++/83993
// { dg-do compile }
// { dg-options "-w" }

int a[5];
extern int b[];
int *const c = &a[6];
int *const d = &b[1];

int
foo ()
{
  return c[-4] + d[-1];
}
