/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-rvrp" } */

#define ADD_NW(A,B) (__extension__({ __typeof(A+B) R; if(__builtin_add_overflow(A,B,&R)) __builtin_unreachable(); R ;}))
_Bool a_b2(unsigned A,  unsigned B) { return ADD_NW(A,B) >= B; }

/* { dg-final { scan-tree-dump "return 1;" "rvrp" } } */

/* FIXME: We need to handle IMAGPART_EXPR going back to the
   .ADD_OVERFLOW. and returning a range of [0, 1].  */
