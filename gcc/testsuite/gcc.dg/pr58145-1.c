/* PR tree-optimization/58145 */
/* { dg-do compile { target { int32plus } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct S { unsigned int data : 32; };
struct T { unsigned int data; };
volatile struct S s2;

void
f1 (int val)
{
  struct S s = { .data = val };
  *(volatile struct S *) 0x880000UL = s;
}

void
f2 (int val)
{
  struct T t = { .data = val };
  *(volatile struct T *) 0x880000UL = t;
}

void
f3 (int val)
{
  *(volatile unsigned int *) 0x880000UL = val;
}

void
f4 (int val)
{
  struct S s = { .data = val };
  s2 = s;
}

/* { dg-final { scan-tree-dump-times " ={v} " 4 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
