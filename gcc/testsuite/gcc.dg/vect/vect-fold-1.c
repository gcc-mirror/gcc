/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */

typedef unsigned char v4qi __attribute__ ((vector_size (4)));

v4qi c;

void foo()
{
  v4qi a = { 1, 2, 3, 4 };
  v4qi b = { 5, 6, 7, 8 };
  c = a + b;
}

/* { dg-final { scan-tree-dump-times "c =.* { 6, 8, 10, 12 }" 1 "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
