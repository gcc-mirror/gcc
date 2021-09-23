/* { dg-do compile } */
/* { dg-options "-O1 -foptimize-sibling-calls -fdump-tree-tailr1-details" } */

struct A {};

struct A foo()
{
  return foo();
}

/* { dg-final { scan-tree-dump-times "Eliminated tail recursion" 1 "tailr1"} } */
