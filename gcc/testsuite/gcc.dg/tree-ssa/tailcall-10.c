/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

struct A {};

struct A goo(void);
struct A foo(void)
{
  return goo();
}

/* { dg-final { scan-tree-dump-times "Found tail call" 1 "tailc"} } */
