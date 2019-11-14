/* { dg-do compile } */
/* { dg-additional-options "-foffload=disable -fdump-tree-gimple" } */
/* { dg-additional-options "-mavx512bw -mavx512vl" { target { i?86-*-* x86_64-*-* } } } */

void f01 (void);
void f02 (void);
#pragma omp declare variant (f01) match (device={isa(avx512f,"avx512vl")})
#pragma omp declare variant (f02) match (device={isa(avx512bw,avx512vl,"avx512f")})
void f03 (void);
void f04 (void);
void f05 (void);
#pragma omp declare variant (f04) match (device={isa(avx512f,avx512vl)})
#pragma omp declare variant (f05) match (device={isa(avx512bw,avx512vl,avx512f)})
void f06 (void);
void f07 (void);
void f08 (void);
#pragma omp declare variant (f07) match (device={isa(sse4,"sse4.1","sse4.2",sse3,"avx")})
#pragma omp declare variant (f08) match (device={isa("avx",sse3)})
void f09 (void);
void f10 (void);
void f11 (void);
void f12 (void);
#pragma omp declare variant (f10) match (device={isa("avx512f")})
#pragma omp declare variant (f11) match (user={condition(1)},device={isa(avx512f)},implementation={vendor(gnu)})
#pragma omp declare variant (f12) match (user={condition(2 + 1)},device={isa(avx512f)})
void f13 (void);
void f14 (void);
void f15 (void);
void f16 (void);
void f17 (void);
#pragma omp declare variant (f14) match (construct={teams,for})
#pragma omp declare variant (f15) match (construct={teams,parallel,for})
#pragma omp declare variant (f16) match (construct={for})
#pragma omp declare variant (f17) match (construct={parallel,for})
void f18 (void);
void f19 (void);
void f20 (void);
void f21 (void);
void f22 (void);
#pragma omp declare variant (f19) match (construct={teams,for})
#pragma omp declare variant (f20) match (construct={teams,parallel,for})
#pragma omp declare variant (f21) match (construct={for})
#pragma omp declare variant (f22) match (construct={parallel,for})
void f23 (void);
void f24 (void);
void f25 (void);
void f26 (void);
#pragma omp declare variant (f24) match (device={kind(cpu)})
#pragma omp declare variant (f25) match (device={kind(cpu),isa(avx512f),arch(x86_64)})
#pragma omp declare variant (f26) match (device={arch(x86_64),kind(cpu)})
void f27 (void);

void
test1 (void)
{
  int i;
  f03 ();	/* { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } } */
		/* { dg-final { scan-tree-dump-times "f03 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } } */
  f09 ();	/* { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } } */
		/* { dg-final { scan-tree-dump-times "f09 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } } */
  f13 ();	/* { dg-final { scan-tree-dump-times "f11 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } } */
		/* { dg-final { scan-tree-dump-times "f13 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } } */
  #pragma omp teams distribute parallel for
  for (i = 0; i < 1; i++)
    f18 ();	/* { dg-final { scan-tree-dump-times "f15 \\\(\\\);" 1 "gimple" } } */
  #pragma omp parallel for
  for (i = 0; i < 1; i++)
    f23 ();	/* { dg-final { scan-tree-dump-times "f22 \\\(\\\);" 1 "gimple" } } */
  f27 ();	/* { dg-final { scan-tree-dump-times "f25 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
		/* { dg-final { scan-tree-dump-times "f24 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && { ! lp64 } } } } } */
		/* { dg-final { scan-tree-dump-times "f24 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* i?86-*-* x86_64-*-* } } } } } */
		/* { dg-final { scan-tree-dump-times "f27 \\\(\\\);" 1 "gimple" { target { nvptx*-*-* amdgcn*-*-* } } } } */
}

#if defined(__i386__) || defined(__x86_64__)
__attribute__((target ("no-avx512bw,avx512f,avx512vl")))
#endif
void
test2 (void)
{
  f06 ();	/* { dg-final { scan-tree-dump-times "f04 \\\(\\\);" 1 "gimple" { target i?86-*-* x86_64-*-* } } } */
		/* { dg-final { scan-tree-dump-times "f06 \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } } */
}
