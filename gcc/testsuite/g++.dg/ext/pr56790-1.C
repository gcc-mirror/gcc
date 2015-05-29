/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */
/* { dg-prune-output "changes the ABI" } */

typedef long vec __attribute__ ((vector_size (2 * sizeof (long))));

vec f (void)
{
  vec a = {  5,  7 };
  vec b = { 11, 13 };
  vec m = { -1,  0 };
  return m ? a : b;
}

/* { dg-final { scan-tree-dump "{ 5, 13 }" "ccp1" } } */
/* { dg-final { scan-tree-dump-not "VEC_COND_EXPR" "ccp1" } } */
