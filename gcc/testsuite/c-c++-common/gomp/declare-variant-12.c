/* { dg-do compile } */
/* { dg-additional-options "-foffload=disable -fdump-tree-gimple" } */
/* { dg-additional-options "-mavx512bw -mavx512vl" { target { i?86-*-* x86_64-*-* } } } */

#pragma omp requires atomic_default_mem_order(seq_cst)
void f01 (void);
void f02 (void);
void f03 (void);
#pragma omp declare variant (f01) match (device={isa("avx512f","avx512vl")}) /* 16 */
#pragma omp declare variant (f02) match (implementation={vendor(score(15):gnu)})
#pragma omp declare variant (f03) match (user={condition(score(11):1)})
void f04 (void);
void f05 (void);
void f06 (void);
void f07 (void);
#pragma omp declare variant (f05) match (device={isa(avx512f,avx512vl)}) /* 16 */
#pragma omp declare variant (f06) match (implementation={vendor(score(15):gnu)})
#pragma omp declare variant (f07) match (user={condition(score(17):1)})
void f08 (void);
void f09 (void);
void f10 (void);
void f11 (void);
void f12 (void);
#pragma omp declare variant (f09) match (device={arch(x86_64)},user={condition(score(65):1)}) /* 64+65 */
#pragma omp declare variant (f10) match (implementation={vendor(score(127):"gnu")})
#pragma omp declare variant (f11) match (device={isa(ssse3)}) /* 128 */
#pragma omp declare variant (f12) match (implementation={atomic_default_mem_order(score(126):seq_cst)})
void f13 (void);
void f14 (void);
void f15 (void);
void f16 (void);
#pragma omp declare variant (f14) match (construct={teams,parallel,for}) /* 1+8+16 */
#pragma omp declare variant (f15) match (construct={parallel},user={condition(score(16):1)}) /* 8+16 */
#pragma omp declare variant (f16) match (implementation={atomic_default_mem_order(score(24):seq_cst)})
void f17 (void);
void f18 (void);
void f19 (void);
void f20 (void);
#pragma omp declare variant (f18) match (construct={teams,parallel,for}) /* 1+8+6 */
#pragma omp declare variant (f19) match (construct={for},user={condition(score(25):1)}) /* 4+25 */
#pragma omp declare variant (f20) match (implementation={atomic_default_mem_order(score(28):seq_cst)})
void f21 (void);
void f22 (void);
void f23 (void);
void f24 (void);
#pragma omp declare variant (f22) match (construct={parallel,for}) /* 8+16 */
#pragma omp declare variant (f23) match (construct={for}) /* 0 */
#pragma omp declare variant (f24) match (implementation={atomic_default_mem_order(score(2):seq_cst)})
void f25 (void);
void f26 (void);
void f27 (void);
void f28 (void);
#pragma omp declare variant (f26) match (construct={parallel,for}) /* 8+16 */
#pragma omp declare variant (f27) match (construct={for},user={condition(score(25):1)}) /* 16 + 25 */
#pragma omp declare variant (f28) match (implementation={atomic_default_mem_order(score(3):seq_cst)})
void f29 (void);

void
test1 (void)
{
  int i, j;
  #pragma omp parallel for	/* 2 constructs in OpenMP context, isa has score 2^4.  */
  for (i = 0; i < 1; i++)
    f04 ();	/* { dg-final { scan-tree-dump-times "f01 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } } */
		/* { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } } */
  #pragma omp target teams	/* 2 constructs in OpenMP context, isa has score 2^4.  */
  f08 ();	/* { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" } } */
  #pragma omp teams
  #pragma omp parallel for
  for (i = 0; i < 1; i++)
    #pragma omp parallel for	/* 5 constructs in OpenMP context, arch is 2^6, isa 2^7.  */
    for (j = 0; j < 1; j++)
      {
	f13 ();	/* { dg-final { scan-tree-dump-times "f09 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
		/* { dg-final { scan-tree-dump-times "f11 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && { ! lp64 } } } } } */
		/* { dg-final { scan-tree-dump-times "f10 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } } */
	f17 ();	/* { dg-final { scan-tree-dump-times "f14 \\\(\\\);" 1 "gimple" } } */
	f21 ();	/* { dg-final { scan-tree-dump-times "f19 \\\(\\\);" 1 "gimple" } } */
      }
  #pragma omp for
  for (i = 0; i < 1; i++)
    #pragma omp parallel for
    for (j = 0; j < 1; j++)
      {
	f25 ();	/* { dg-final { scan-tree-dump-times "f22 \\\(\\\);" 1 "gimple" } } */
	f29 ();	/* { dg-final { scan-tree-dump-times "f27 \\\(\\\);" 1 "gimple" } } */
      }
}
