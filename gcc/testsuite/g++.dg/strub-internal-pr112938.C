/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized -O2" } */
/* { dg-require-effective-target strub } */

bool __attribute__ ((__strub__ ("internal")))
f(bool i, volatile bool j)
{
  return (i ^ j) == j;
}

/* Check for two dereferences of the indirected volatile j parm.  */
/* { dg-final { scan-tree-dump-times {={v} \*j_[0-9][0-9]*(D)} 2 "optimized" } } */
