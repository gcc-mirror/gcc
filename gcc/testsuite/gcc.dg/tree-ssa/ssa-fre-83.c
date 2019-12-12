/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

struct X
{
   int a : 1;
   int b : 1;
} x;

void foo (int v)
{
  x.a = 1;
  x.b = v;
  x.a = 1;
  x.b = v;
}

struct Y
{
   _Bool a;
   _Bool b;
} y;

void bar (int v)
{
  y.a = 1;
  y.b = v;
  y.a = 1;
  y.b = v;
}

/* { dg-final { scan-tree-dump-times "Deleted redundant store" 4 "fre1" } } */
