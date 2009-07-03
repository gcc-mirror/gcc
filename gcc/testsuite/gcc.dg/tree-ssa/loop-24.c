/* { dg-do compile } */
/* { dg-options "-O2 -fstrict-overflow -fdump-tree-optimized" } */

void foo(int a, int b)
{ for(;a!=b;a+=4); }

void foo2(int a, int b)
{ for(;a<b;a+=4); }

void foo3(int*a, int* b)
{ for(;a<b;a++); }

void foo4(int*a, int*b)
{ for(;a!=b;a++); }

/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
