/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

#define DECLS(n,VOL)			\
__attribute__((noinline,noclone))	\
_Bool h##n(_Bool A,_Bool B){			\
    VOL _Bool C = A | B;			\
    VOL _Bool D = A == B;			\
    return C & D;			\
}					\
__attribute__((noinline,noclone))	\
_Bool i##n(_Bool A,_Bool B){			\
    VOL _Bool C = A == B;			\
    return A | C;			\
}					\
__attribute__((noinline,noclone))	\
_Bool k##n(_Bool A,_Bool B){			\
    VOL _Bool C = A & B;			\
    VOL _Bool D = A == B;			\
    return C | D;			\
}					\

DECLS(0,)
DECLS(1,volatile)

int main(){
    for(int A = 0; A <= 1; ++A)
      for(int B = 0; B <= 1; ++B)
	{
	  if (h0 (A, B) != h1 (A, B)) __builtin_abort();
	  if (i0 (A, B) != i1 (A, B)) __builtin_abort();
	  if (k0 (A, B) != k1 (A, B)) __builtin_abort();
	}
}

/* { dg-final { scan-tree-dump-times "bit_not_expr," 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_and_expr," 3 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr," 4 "optimized"} } */
/* { dg-final { scan-tree-dump-times "eq_expr,"      4 "optimized"} } */
/* { dg-final { scan-tree-dump-times "ne_expr,"      7 "optimized"} } */
/* { dg-final { scan-tree-dump-not   "bit_xor_expr,"   "optimized"} } */
