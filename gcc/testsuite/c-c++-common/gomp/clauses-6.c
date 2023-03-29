/* { dg-do compile } */
/* { dg-additional-options "-std=c99" { target c } } */

typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
{
  omp_null_allocator = 0,
  omp_default_mem_alloc = 1,
  omp_large_cap_mem_alloc = 2,
  omp_const_mem_alloc = 3,
  omp_high_bw_mem_alloc = 4,
  omp_low_lat_mem_alloc = 5,
  omp_cgroup_mem_alloc = 6,
  omp_pteam_mem_alloc = 7,
  omp_thread_mem_alloc = 8,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

typedef enum omp_sync_hint_t {
omp_sync_hint_none = 0x0,
omp_lock_hint_none = omp_sync_hint_none,
omp_sync_hint_uncontended = 0x1,
omp_lock_hint_uncontended = omp_sync_hint_uncontended,
omp_sync_hint_contended = 0x2,
omp_lock_hint_contended = omp_sync_hint_contended,
omp_sync_hint_nonspeculative = 0x4,
omp_lock_hint_nonspeculative = omp_sync_hint_nonspeculative,
omp_sync_hint_speculative = 0x8,
omp_lock_hint_speculative = omp_sync_hint_speculative
} omp_sync_hint_t;
  
typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;

int t;
#pragma omp threadprivate (t)

#pragma omp declare target
int f, l, ll, r, r2;

void
foo (int d, int m, int i1, int i2, int p, int *idp, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int ntm)
{
  #pragma omp distribute parallel for, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), order(concurrent), allocate (omp_default_mem_alloc:f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute parallel for simd, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), if(simd: i1), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), nontemporal(ntm), \
    safelen(8), simdlen(4), aligned(q: 32), order(concurrent), allocate (omp_default_mem_alloc:f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute simd, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    safelen(8), simdlen(4), aligned(q: 32), reduction(+:r), if(i1), nontemporal(ntm), \
    order(concurrent), allocate (omp_default_mem_alloc:f)
  for (int i = 0; i < 64; i++)
    ll++;
}

void
qux (int p)
{
  #pragma omp loop, bind(teams), order(concurrent), \
    private (p), lastprivate (l), collapse(1), reduction(+:r)
  for (l = 0; l < 64; ++l)
    ll++;
}
#pragma omp end declare target

void
baz (int d, int m, int i1, int i2, int p, int *idp, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int ntm)
{
  #pragma omp distribute parallel for, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), copyin(t), allocate (p)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute parallel for, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), order(concurrent), allocate (p)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute parallel for simd, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), if(simd: i1), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), nontemporal(ntm), \
    safelen(8), simdlen(4), aligned(q: 32), copyin(t), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute parallel for simd, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), if(simd: i1), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), nontemporal(ntm), \
    safelen(8), simdlen(4), aligned(q: 32), order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute simd, \
    private (p), firstprivate (f), collapse(1), dist_schedule(static, 16), \
    safelen(8), simdlen(4), aligned(q: 32), reduction(+:r), if(i1), nontemporal(ntm), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp loop, bind(parallel), order(concurrent), \
    private (p), lastprivate (l), collapse(1), reduction(+:r)
  for (l = 0; l < 64; ++l)
    ll++;
}

void
bar (int d, int m, int i1, int i2, int i3, int p, int *idp, int hda, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int *dd, int ntm,
     int n1, int n2)
{
  #pragma omp for simd, \
    private (p), firstprivate (f), lastprivate (l), linear (ll:1), reduction(+:r), schedule(static, 4), collapse(1), nowait, \
    safelen(8), simdlen(4), aligned(q: 32), nontemporal(ntm), if(i1), order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel for, \
    private (p), firstprivate (f), if (parallel: i2), default(shared), shared(s), copyin(t), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), linear (ll:1), ordered, schedule(static, 4), collapse(1), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel for, \
    private (p), firstprivate (f), if (parallel: i2), default(shared), shared(s), copyin(t), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), linear (ll:1), schedule(static, 4), collapse(1), order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel for simd, \
    private (p), firstprivate (f), if (i2), default(shared), shared(s), copyin(t), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), linear (ll:1), schedule(static, 4), collapse(1), \
    safelen(8), simdlen(4), aligned(q: 32), nontemporal(ntm), order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel sections, \
    private (p), firstprivate (f), if (parallel: i2), default(shared), shared(s), copyin(t), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), allocate (f)
  {
    #pragma omp section
    {}
    #pragma omp section
    {}
  }
  #pragma omp target parallel, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    nowait, depend(inout: dd[0]), allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
    ;
  #pragma omp target parallel for, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), linear (ll:1), ordered, schedule(static, 4), collapse(1), nowait, depend(inout: dd[0]), \
    allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target parallel for, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), linear (ll:1), schedule(static, 4), collapse(1), nowait, depend(inout: dd[0]), order(concurrent), \
    allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target parallel for simd, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), linear (ll:1), schedule(static, 4), collapse(1), \
    safelen(8), simdlen(4), aligned(q: 32), nowait, depend(inout: dd[0]), nontemporal(ntm), if (simd: i3), order(concurrent), \
    allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target teams, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    shared(s), default(shared), reduction(+:r), num_teams(nte - 1:nte), thread_limit(tl), nowait depend(inout: dd[0]), \
    allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
    ;
  #pragma omp target teams distribute, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), order(concurrent), \
    collapse(1), dist_schedule(static, 16), nowait, depend(inout: dd[0]), allocate (omp_default_mem_alloc:f), in_reduction(+:r2), \
    has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ;
  #pragma omp target teams distribute parallel for, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), nowait, depend(inout: dd[0]), order(concurrent), \
     allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target teams distribute parallel for simd, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), order(concurrent), \
    safelen(8), simdlen(4), aligned(q: 32), nowait, depend(inout: dd[0]), nontemporal(ntm), if (simd: i3), \
    allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target teams distribute simd, \
    device(d), map (tofrom: m), if (i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), order(concurrent), \
    safelen(8), simdlen(4), aligned(q: 32), nowait depend(inout: dd[0]), nontemporal(ntm), \
    allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target simd, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    safelen(8), simdlen(4), lastprivate (l), linear(ll: 1), aligned(q: 32), reduction(+:r), \
    nowait depend(inout: dd[0]), nontemporal(ntm), if(simd:i3), order(concurrent), \
    allocate (omp_default_mem_alloc:f), in_reduction(+:r2), has_device_addr(hda)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction(+:r2), allocate (r2)
  #pragma omp taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), if(simd: i2), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), in_reduction(+:r2), nontemporal(ntm), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction(+:r), allocate (r)
  #pragma omp taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(i1), final(fi), mergeable, nogroup, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), in_reduction(+:r), nontemporal(ntm), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskwait
  #pragma omp taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), if(taskloop: i1), final(fi), priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(+:r), if (simd: i3), nontemporal(ntm), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target, nowait, depend(inout: dd[0]), in_reduction(+:r2)
  #pragma omp teams distribute, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), allocate (omp_default_mem_alloc: f), order(concurrent)
  for (int i = 0; i < 64; i++)
    ;
  #pragma omp target
  #pragma omp teams distribute parallel for, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), order(concurrent), allocate (omp_default_mem_alloc: f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target
  #pragma omp teams distribute parallel for simd, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), order(concurrent), \
    safelen(8), simdlen(4), aligned(q: 32), if (simd: i3), nontemporal(ntm), \
    allocate (omp_default_mem_alloc: f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target
  #pragma omp teams distribute simd, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), order(concurrent), \
    safelen(8), simdlen(4), aligned(q: 32), if(i3), nontemporal(ntm), \
    allocate (omp_default_mem_alloc: f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute parallel for, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), copyin(t), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute parallel for, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), order(concurrent), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute parallel for simd, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), \
    safelen(8), simdlen(4), aligned(q: 32), if (simd: i3), nontemporal(ntm), copyin(t), \
    allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute parallel for simd, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), \
    if (parallel: i2), num_threads (nth), proc_bind(spread), \
    lastprivate (l), schedule(static, 4), order(concurrent), \
    safelen(8), simdlen(4), aligned(q: 32), if (simd: i3), nontemporal(ntm), \
    allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute simd, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), \
    collapse(1), dist_schedule(static, 16), order(concurrent), \
    safelen(8), simdlen(4), aligned(q: 32), if(i3), nontemporal(ntm), allocate(f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master, \
    private (p), firstprivate (f), if (parallel: i2), default(shared), shared(s), reduction(+:r), \
    num_threads (nth), proc_bind(spread), copyin(t), allocate (f)
    ;
  #pragma omp parallel masked, \
    private (p), firstprivate (f), if (parallel: i2), default(shared), shared(s), reduction(+:r), \
    num_threads (nth), proc_bind(spread), copyin(t), allocate (f), filter (d)
    ;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp master taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), in_reduction(+:r2), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp masked taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), in_reduction(+:r2), allocate (f), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp master taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), if(simd: i2), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), in_reduction(+:r2), nontemporal(ntm), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp masked taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), if(simd: i2), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), in_reduction(+:r2), nontemporal(ntm), \
    order(concurrent), allocate (f), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), if (parallel: i2), num_threads (nth), proc_bind(spread), copyin(t), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel masked taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), if (parallel: i2), num_threads (nth), proc_bind(spread), copyin(t), allocate (f), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), if(simd: i2), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), nontemporal(ntm), if (parallel: i2), num_threads (nth), proc_bind(spread), copyin(t), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel masked taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), grainsize (g), collapse(1), untied, if(taskloop: i1), if(simd: i2), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), nontemporal(ntm), if (parallel: i2), num_threads (nth), proc_bind(spread), copyin(t), \
    order(concurrent), allocate (f), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp master taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), in_reduction(+:r2)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp mastked taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), in_reduction(+:r2), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp master taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), in_reduction(+:r2), nontemporal(ntm), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup, task_reduction (+:r2), allocate (r2)
  #pragma omp masked taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), in_reduction(+:r2), nontemporal(ntm), \
    order(concurrent), allocate (f), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), num_threads (nth), proc_bind(spread), copyin(t), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel masked taskloop, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    reduction(default, +:r), num_threads (nth), proc_bind(spread), copyin(t), allocate (f), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), nontemporal(ntm), num_threads (nth), proc_bind(spread), copyin(t), \
    order(concurrent), allocate (f)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel masked taskloop simd, \
    private (p), firstprivate (f), lastprivate (l), shared (s), default(shared), num_tasks (nta), collapse(1), untied, if(i1), final(fi), mergeable, priority (pp), \
    safelen(8), simdlen(4), linear(ll: 1), aligned(q: 32), reduction(default, +:r), nontemporal(ntm), num_threads (nth), proc_bind(spread), copyin(t), \
    order(concurrent), allocate (f), filter (d)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp loop, bind(thread), order(concurrent), \
    private (p), lastprivate (l), collapse(1), reduction(+:r)
  for (l = 0; l < 64; ++l)
    ll++;
  #pragma omp parallel loop, \
    private (p), firstprivate (f), if (parallel: i2), default(shared), shared(s), copyin(t), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), collapse(1), bind(parallel), order(concurrent), allocate (f)
  for (l = 0; l < 64; l++)
    ll++;
  #pragma omp parallel loop, \
    private (p), firstprivate (f), if (parallel: i2), default(shared), shared(s), copyin(t), reduction(+:r), num_threads (nth), proc_bind(spread), \
    lastprivate (l), collapse(1), allocate (f)
  for (l = 0; l < 64; l++)
    ll++;
  #pragma omp teams loop, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), \
    collapse(1), lastprivate (l), bind(teams), allocate (f)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp teams loop, \
    private(p), firstprivate (f), shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), \
    collapse(1), lastprivate (l), order(concurrent), allocate (f)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target parallel loop, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    nowait depend(inout: dd[0]), lastprivate (l), bind(parallel), order(concurrent), collapse(1), \
    allocate (omp_default_mem_alloc: f), in_reduction(+:r2), has_device_addr(hda)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target parallel loop, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    if (parallel: i2), default(shared), shared(s), reduction(+:r), num_threads (nth), proc_bind(spread), \
    nowait depend(inout: dd[0]), lastprivate (l), order(concurrent), collapse(1), \
    allocate (omp_default_mem_alloc: f), in_reduction(+:r2), has_device_addr(hda)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target teams loop, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    shared(s), default(shared), reduction(+:r), num_teams(nte-1:nte), thread_limit(tl), nowait depend(inout: dd[0]), \
    lastprivate (l), bind(teams), collapse(1), \
    allocate (omp_default_mem_alloc: f), in_reduction(+:r2), has_device_addr(hda)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target teams loop, \
    device(d), map (tofrom: m), if (target: i1), private (p), firstprivate (f), defaultmap(tofrom: scalar), is_device_ptr (idp), \
    shared(s), default(shared), reduction(+:r), num_teams(nte), thread_limit(tl), nowait depend(inout: dd[0]), \
    lastprivate (l), order(concurrent), collapse(1), \
    allocate (omp_default_mem_alloc: f), in_reduction(+:r2), has_device_addr(hda)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp critical
  ;
  #pragma omp critical (foobar),hint(omp_sync_hint_none)
  ;
  #pragma omp taskwait, depend (inout: dd[0])
  ;
  #pragma omp taskgroup, task_reduction(+:r2),allocate (r2)
  ;
  #pragma omp atomic, update,seq_cst,hint(omp_sync_hint_none)
  p++;
  #pragma omp atomic, read, hint(omp_sync_hint_none),relaxed
  f = p;
  #pragma omp atomic,write, release hint(omp_sync_hint_none)
  p = f;
  #pragma omp flush
  ;
  #pragma omp flush, acq_rel
  ;
  #pragma omp flush, acquire
  ;
  #pragma omp flush, release
  ;
  #pragma omp flush, seq_cst
  ;
  #pragma omp flush (p, f)
  ;
  #pragma omp simd, \
    private (p),lastprivate (l),linear (ll:1),reduction(+:r),collapse(1),safelen(8),simdlen(4),aligned(q: 32), \
    nontemporal(ntm),if(i1)
  for (int i = 0; i < 64; i++)
    #pragma omp ordered, simd
      ll++;
  #pragma omp for, \
    private (p),firstprivate (f),lastprivate (l),linear (ll:1),reduction(+:r),schedule(static, 4),collapse(1),nowait, \
    ordered, allocate (f)
  for (int i = 0; i < 64; i++)
    #pragma omp ordered, threads
      ll++;
  #pragma omp for, ordered (1)
  for (l = 0; l < 64; l++)
    {
      #pragma omp ordered, depend (sink: l - 1)
      ;
      #pragma omp ordered, depend (source)
      ;
    }
  extern omp_depend_t depobj;
  #pragma omp depobj(depobj),depend(in : dd[0])
  ;
  #pragma omp parallel
  {
    if (p) {
      #pragma omp cancel, parallel
      ;
    } else {
      #pragma omp cancellation point, parallel
      ;
    }
  }
  #pragma omp scope, private (p), firstprivate (f), reduction(+:r), nowait, \
    allocate(omp_default_mem_alloc: r)
    ;
  #pragma omp scope, private (p), firstprivate (f), reduction(task, +:r), \
    allocate (omp_default_mem_alloc: f)
    ;
  extern int t2;
  #pragma omp threadprivate (t2)
  extern int t2;
  #pragma omp declare reduction (dr: int: omp_out += omp_in),initializer (omp_priv = 0)
  #pragma omp assume, no_openmp, no_openmp_routines, no_parallelism, \
		      absent (atomic, barrier, cancel, cancellation point), \
		      absent (critical, depobj), \
		      absent (distribute, flush, loop, masked, master, nothing, ordered), \
		      absent (parallel, scan, scope, section, sections, simd, single, task), \
		      absent (taskgroup, taskloop, taskwait, taskyield), \
		      absent (target, teams, for, error), holds (n1 < n2)
  if (0)
    ;
  #pragma omp assume, contains (simd)
  #pragma omp for simd
  for (int i = 0; i < 64; i++)
    ;
}

void corge1 ();

void
corge ()
{
  #pragma omp declare variant (corge1),match (construct={parallel,for})
  extern void corge2 ();
  #pragma omp parallel
  #pragma omp  for
  for (int i = 0; i < 5; i++)
    corge2 ();
  #pragma omp declare simd, simdlen(4),linear(l),aligned(p:4),uniform(p),inbranch
  #pragma omp declare simd,simdlen(8),notinbranch
  extern int corge3 (int l, int *p);
  #pragma omp declare simd, simdlen(4),linear(l),aligned(p:4),uniform(p),inbranch
  #pragma omp declare simd, simdlen(8),notinbranch
  extern int corge4 (int l, int *p);
  #pragma omp declare simd, simdlen(4),linear(l),aligned(p:4),uniform(p),inbranch
  #pragma omp declare simd, simdlen(8),notinbranch
  extern int corge5 (int l, int *p);
  #pragma omp declare target
  extern void corge6 ();
  #pragma omp end declare target
}

int
garply (int a, int *c, int *d, int *e, int *f)
{
  int i;
  #pragma omp simd, reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan, exclusive (a)
      a += c[i];
    }
  #pragma omp simd, reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      #pragma omp scan inclusive (a)
      d[i] = a;
    }
  return a;
}
