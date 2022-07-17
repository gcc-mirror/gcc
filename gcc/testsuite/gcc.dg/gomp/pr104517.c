/* PR debug/104517 */
/* { dg-do compile } */
/* { dg-options "-O1 -fcompare-debug -fopenmp -fno-tree-ter -save-temps" } */

typedef enum omp_allocator_handle_t
{
  omp_null_allocator = 0,
  omp_default_mem_alloc = 1,
  omp_large_cap_mem_alloc = 2,
  omp_const_mem_alloc = 3,
  omp_high_bw_mem_alloc = 4,
} omp_allocator_handle_t;

int t, bar_nte, bar_tl, bar_i3, bar_dd;

#pragma omp threadprivate(t)
#pragma omp declare target
int f, l, ll, r, r2;
#pragma omp end declare target

void
bar (int *idp, int s, int nth, int g, int nta, int fi, int pp, int *q,
     int ntm)
{
  int p = 0, i2 = 0, i1 = 0, m = 0, d = 0;

#pragma omp target parallel for                               \
  device(p) firstprivate (f) allocate (omp_default_mem_alloc:f) in_reduction(+:r2)
  for (int i = 0; i < 4; i++)
    ll++;

#pragma omp target parallel for                                         \
  device(d) map (m)                                                     \
  if (target: p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
  if (parallel: i2) reduction(+:r) num_threads (nth) linear (ll)        \
  schedule(static) collapse(1) nowait depend(inout: d) \
  allocate (omp_default_mem_alloc:f) in_reduction(+:r2)
  for (int i = 0; i < 4; i++)
    ll++;

#pragma omp taskloop simd firstprivate(f) lastprivate(s) grainsize(g) \
  collapse(1) untied if (i1) final(fi) mergeable nogroup              \
  priority(pp) linear(ll) aligned(q) allocate(f)
  for (int i = 0; i < 4; i++)
    ll++;

#pragma omp taskloop simd firstprivate(f) lastprivate(s) num_tasks(nta) \
  collapse(1) if (i1) final(fi) priority(pp) safelen(8) simdlen(4)      \
  linear(ll) aligned(q) nontemporal(ntm) order(concurrent) allocate(f)
  for (int i = 0; i < 4; i++)
    ll++;

#pragma omp parallel master firstprivate(f) shared(nth) proc_bind(spread) \
  copyin(t) allocate(f)
  ;
}
