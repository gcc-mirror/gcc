/* PR tree-optimization/101650 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* (A | C) == A should become (A & C) != 0 when C is a power of 2.  */
_Bool f_ior_eq(unsigned len) {
  const unsigned N = 4;
  unsigned newlen = len | N;
  return newlen == len;
}

/* (A | C) != A should become (A & C) == 0 when C is a power of 2.  */
_Bool f_ior_ne(unsigned len) {
  const unsigned N = 4;
  unsigned newlen = len | N;
  return newlen != len;
}

/* { dg-final { scan-tree-dump-not "\\| 4" "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 4" 2 "optimized" } } */
