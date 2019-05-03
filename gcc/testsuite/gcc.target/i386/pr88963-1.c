/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx2 -fdump-tree-optimized" } */

typedef int VInt __attribute__((vector_size(64)));

void test(VInt*__restrict a, VInt*__restrict b, 
	  VInt*__restrict c)
{
  *a = *b + *c;
}

/* Vector loads and stores should be split.  */
/* { dg-final { scan-tree-dump-not "vector\\(16\\)" "optimized" } } */
