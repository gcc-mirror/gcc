/* { dg-do compile { target c } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

void f01 (void);
#pragma omp declare variant (f01) match (user={condition(6 == 7)},implementation={vendor(gnu)})
void f02 (void);
void f03 (void);
#pragma omp declare variant (f03) match (user={condition(6 == 6)},implementation={atomic_default_mem_order(seq_cst)})
void f04 (void);
void f05 (void);
#pragma omp declare variant (f05) match (user={condition(1)},implementation={atomic_default_mem_order(relaxed)})
void f06 (void);
#pragma omp requires atomic_default_mem_order(seq_cst)
void f07 (void);
#pragma omp declare variant (f07) match (construct={parallel,for},device={kind("any")})
void f08 (void);
void f09 (void);
#pragma omp declare variant (f09) match (construct={parallel,for},implementation={vendor("gnu")})
void f10 (void);
void f11 (void);
#pragma omp declare variant (f11) match (construct={parallel,for})
void f12 (void);
void f13 (void);
#pragma omp declare variant (f13) match (construct={parallel,for})
void f14 (void);
#pragma omp declare target to (f13, f14)
void f15 (void);
#pragma omp declare variant (f15) match (implementation={vendor(llvm)})
void f16 (void);
void f17 (void);
#pragma omp declare variant (f17) match (construct={target,parallel})
void f18 (void);
void f19 (void);
#pragma omp declare variant (f19) match (construct={target,parallel})
void f20 (void);
void f21 (void);
#pragma omp declare variant (f21) match (construct={teams,parallel})
void f22 (void);
void f23 (void);
#pragma omp declare variant (f23) match (construct={teams,parallel,for})
void f24 (void);
void f25 (void);
#pragma omp declare variant (f25) match (construct={teams,parallel})
void f26 (void);
void f27 (void);
#pragma omp declare variant (f27) match (construct={teams,parallel,for})
void f28 (void);
void f29 (void);
#pragma omp declare variant (f29) match (implementation={vendor(gnu)})
void f30 (void);
void f31 (void);
#pragma omp declare variant (f31) match (construct={teams,parallel,for})
void f32 (void);
void f33 (void);
#pragma omp declare variant (f33) match (device={kind("any\0any")})	/* { dg-warning "unknown property '.any.000any.' of 'kind' selector" } */
void f34 (void);
void f35 (void);
#pragma omp declare variant (f35) match (implementation={vendor("gnu\0")})	/* { dg-warning "unknown property '.gnu.000.' of 'vendor' selector" } */
void f36 (void);

void
test1 (void)
{
  int i;
  f02 ();	/* { dg-final { scan-tree-dump-times "f02 \\\(\\\);" 1 "gimple" } } */
  f04 ();	/* { dg-final { scan-tree-dump-times "f03 \\\(\\\);" 1 "gimple" } } */
  f06 ();	/* { dg-final { scan-tree-dump-times "f06 \\\(\\\);" 1 "gimple" } } */
  #pragma omp parallel
  #pragma omp for
  for (i = 0; i < 1; i++)
    f08 ();	/* { dg-final { scan-tree-dump-times "f07 \\\(\\\);" 1 "gimple" } } */
  #pragma omp parallel for
  for (i = 0; i < 1; i++)
    f10 ();	/* { dg-final { scan-tree-dump-times "f09 \\\(\\\);" 1 "gimple" } } */
  #pragma omp for
  for (i = 0; i < 1; i++)
    #pragma omp parallel
    f12 ();	/* { dg-final { scan-tree-dump-times "f12 \\\(\\\);" 1 "gimple" } } */
  #pragma omp parallel
  #pragma omp target
  #pragma omp for
  for (i = 0; i < 1; i++)
    f14 ();	/* { dg-final { scan-tree-dump-times "f14 \\\(\\\);" 1 "gimple" } } */
  f16 ();	/* { dg-final { scan-tree-dump-times "f16 \\\(\\\);" 1 "gimple" } } */
  f34 ();	/* { dg-final { scan-tree-dump-times "f34 \\\(\\\);" 1 "gimple" } } */
  f36 ();	/* { dg-final { scan-tree-dump-times "f36 \\\(\\\);" 1 "gimple" } } */
}

#pragma omp declare target
void
test2 (void)
{
  #pragma omp parallel
  f18 ();	/* { dg-final { scan-tree-dump-times "f17 \\\(\\\);" 1 "gimple" } } */
}
#pragma omp end declare target

void test3 (void);
#pragma omp declare target to (test3)

void
test3 (void)
{
  #pragma omp parallel
  f20 ();	/* { dg-final { scan-tree-dump-times "f20 \\\(\\\);" 1 "gimple" } } */
}

void
f21 (void)
{
  int i;
  #pragma omp for
  for (i = 0; i < 1; i++)
    f24 ();	/* { dg-final { scan-tree-dump-times "f23 \\\(\\\);" 1 "gimple" } } */
}

void
f26 (void)
{
  int i;
  #pragma omp for
  for (i = 0; i < 1; i++)
    f28 ();	/* { dg-final { scan-tree-dump-times "f28 \\\(\\\);" 1 "gimple" } } */
}

void
f29 (void)
{
  int i;
  #pragma omp for
  for (i = 0; i < 1; i++)
    f32 ();	/* { dg-final { scan-tree-dump-times "f32 \\\(\\\);" 1 "gimple" } } */
}
