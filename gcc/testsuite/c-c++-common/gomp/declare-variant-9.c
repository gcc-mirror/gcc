/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */
/* { dg-additional-options "-mno-sse3" { target { i?86-*-* x86_64-*-* } } } */

#undef i386
void f01 (void);
#pragma omp declare variant (f01) match (device={isa("avx512f",avx512bw)})
void f02 (void);
void f03 (void);
#pragma omp declare variant (f03) match (device={arch(x86_64),isa("avx512f","avx512bw")})
void f04 (void);
void f05 (void);
#pragma omp declare variant (f05) match (device={kind(gpu)})
void f06 (void);
void f07 (void);
#pragma omp declare variant (f07) match (device={kind("cpu")})
void f08 (void);
void f09 (void);
#pragma omp declare variant (f09) match (device={isa(sm_35)})
void f10 (void);
void f11 (void);
#pragma omp declare variant (f11) match (device={arch(nvptx)})
void f12 (void);
void f13 (void);
#pragma omp declare variant (f13) match (device={arch("i386"),isa(sse4)})
void f14 (void);
void f15 (void);
#pragma omp declare variant (f15) match (device={isa(sse4,ssse3),arch(i386)})
void f16 (void);
void f17 (void);
#pragma omp declare variant (f17) match (device={kind("fpga")})
void f18 (void);

void
test1 (void)
{
  int i;
  f02 ();	/* { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" } } */
  f14 ();	/* { dg-final { scan-tree-dump-times "f14 \\\(\\\);" 1 "gimple" } } */
  f18 ();	/* { dg-final { scan-tree-dump-times "f18 \\\(\\\);" 1 "gimple" } } */
}

#if defined(__i386__) || defined(__x86_64__)
__attribute__((target ("avx512f,avx512bw")))
#endif
void
test2 (void)
{
  f04 ();	/* { dg-final { scan-tree-dump-times "f03 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
		/* { dg-final { scan-tree-dump-times "f04 \\\(\\\);" 1 "gimple" { target { { ! lp64 } || { ! { i?86-*-* x86_64-*-* } } } } } } */
  f16 ();	/* { dg-final { scan-tree-dump-times "f15 \\\(\\\);" 1 "gimple" { target ia32 } } } */
		/* { dg-final { scan-tree-dump-times "f16 \\\(\\\);" 1 "gimple" { target { ! ia32 } } } } */
}

void
test3 (void)
{
  f06 ();	/* { dg-final { scan-tree-dump-times "f06 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } } */
  f08 ();	/* { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } } */
  f10 ();	/* { dg-final { scan-tree-dump-times "f10 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* amdgcn*-*-* } } } } } */
  f12 ();	/* { dg-final { scan-tree-dump-times "f12 \\\(\\\);" 1 "gimple" { target { ! { nvptx*-*-* } } } } } */
		/* { dg-final { scan-tree-dump-times "f11 \\\(\\\);" 1 "gimple" { target { nvptx*-*-* } } } } */
}
