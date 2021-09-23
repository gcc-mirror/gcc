/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

struct A {};

void goo(void);
struct A foo(void)
{
  goo();
}

/* { dg-final { scan-tree-dump-times "Found tail call" 1 "tailc"} } */
