/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

unsigned f(unsigned i){
  i >>= __SIZEOF_INT__ * __CHAR_BIT__ - 3;
  i = ~i;
  return i & 7;
}

/* { dg-final { scan-tree-dump "bit_xor_expr" "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_not_expr" "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_and_expr" "optimized" } } */
