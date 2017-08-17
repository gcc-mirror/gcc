/* { dg-do compile } */
/* { dg-options "-O -msse2 -fdump-tree-ccp1" } */

typedef int v4si __attribute__((vector_size(16)));

float f;

v4si foo (v4si a)
{
  __builtin_memcpy ((char *)&a + 4, &f, 4);
  return a;
}

/* { dg-final { scan-tree-dump "Now a gimple register: a" "ccp1" } } */
/* { dg-final { scan-tree-dump "BIT_INSERT_EXPR <a" "ccp1" } } */
