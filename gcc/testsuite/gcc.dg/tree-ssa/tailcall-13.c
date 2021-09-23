/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

struct A {};
struct B{};

struct B goo(void);
struct A foo(void)
{
  struct A a;
  goo();
  return a;
}

/* { dg-final { scan-tree-dump-times "Found tail call" 1 "tailc"} } */
