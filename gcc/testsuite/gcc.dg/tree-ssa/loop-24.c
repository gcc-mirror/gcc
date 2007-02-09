/* { dg-do compile } */
/* { dg-options "-O -fstrict-overflow -fdump-tree-empty" } */

void foo(int a, int b)
{ for(;a!=b;a+=4); }

void foo2(int a, int b)
{ for(;a<b;a+=4); }

void foo3(int*a, int* b)
{ for(;a<b;a++); }

void foo4(int*a, int*b)
{ for(;a!=b;a++); }

/* { dg-final { scan-tree-dump-times "Removing empty loop" 4 "empty" } } */
/* { dg-final { cleanup-tree-dump "empty" } } */
