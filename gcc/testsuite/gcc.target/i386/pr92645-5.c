/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1 -mavx2 -Wno-psabi" } */
typedef long long v4di __attribute__((vector_size(32)));
struct Vec
{
  unsigned int v[8];
};

v4di pun (struct Vec *s)
{
  v4di tem;
  __builtin_memcpy (&tem, s, 32);
  return tem;
}

/* We're expecting exactly two stmts, in particular no BIT_INSERT_EXPR
   and no memcpy call.
    _3 = MEM <vector(4) long long int> [(char * {ref-all})s_2(D)];
    return _3;  */
/* { dg-final { scan-tree-dump-times " = MEM" 1 "cddce1" } } */
/* { dg-final { scan-tree-dump-not "memcpy" "cddce1" } } */
