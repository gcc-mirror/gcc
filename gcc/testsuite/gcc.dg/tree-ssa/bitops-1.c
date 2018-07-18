/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

#define DECLS(n,VOL)			\
__attribute__((noinline,noclone))	\
int f##n(int A,int B){			\
    VOL int C = A & ~B;			\
    VOL int D = A ^ B;			\
    return C | D;			\
}					\
__attribute__((noinline,noclone))	\
int g##n(int A,int B){			\
    VOL int C = A & ~B;			\
    return C ^ ~A;			\
}					\
__attribute__((noinline,noclone))	\
int h##n(int A,int B){			\
    VOL int C = A | B;			\
    VOL int D = A ^ B;			\
    return C & ~D;			\
}					\
__attribute__((noinline,noclone))	\
int i##n(int A,int B){			\
    VOL int C = A ^ B;			\
    return A | ~C;			\
}					\
__attribute__((noinline,noclone))	\
int J##n(int A,int B){			\
    VOL int C = A | B;			\
    VOL int D = A & B;			\
    return C | D;			\
}					\
__attribute__((noinline,noclone))	\
int k##n(int A,int B){			\
    VOL int C = A & B;			\
    VOL int D = A ^ B;			\
    return C | ~D;			\
}					\
__attribute__((noinline,noclone))	\
int l##n(int A,int B){			\
    VOL int C = A & ~B;			\
    return ~C;				\
}					\
__attribute__((noinline,noclone))	\
int m##n(int A,int B){			\
    VOL int C = A & B;			\
    VOL int D = A ^ B;			\
    return C ^ D;			\
}

DECLS(0,)
DECLS(1,volatile)

int main(){
    for(int A = 0; A <= 1; ++A)
      for(int B = 0; B <= 1; ++B)
	{
	  if (f0 (A, B) != f1 (A, B)) __builtin_abort();
	  if (g0 (A, B) != g1 (A, B)) __builtin_abort();
	  if (h0 (A, B) != h1 (A, B)) __builtin_abort();
	  if (i0 (A, B) != i1 (A, B)) __builtin_abort();
	  if (J0 (A, B) != J1 (A, B)) __builtin_abort();
	  if (k0 (A, B) != k1 (A, B)) __builtin_abort();
	  if (l0 (A, B) != l1 (A, B)) __builtin_abort();
	  if (m0 (A, B) != m1 (A, B)) __builtin_abort();
	}
}

/* { dg-final { scan-tree-dump-times "bit_not_expr" 12 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_and_expr"  9 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr" 10 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr"  9 "optimized"} } */
