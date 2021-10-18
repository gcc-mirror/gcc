// { dg-do compile { target c++17 } }

typedef enum omp_allocator_handle_t
: __UINTPTR_TYPE__
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
  [[omp::directive (distribute parallel for,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),order(concurrent),allocate (omp_default_mem_alloc:f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (distribute parallel for simd,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    if (parallel: i2),if(simd: i1),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),nontemporal(ntm),
    safelen(8),simdlen(4),aligned(q: 32),order(concurrent),allocate (omp_default_mem_alloc:f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (distribute simd,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    safelen(8),simdlen(4),aligned(q: 32),reduction(+:r),if(i1),nontemporal(ntm),
    order(concurrent),allocate (omp_default_mem_alloc:f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (distribute,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    allocate (omp_default_mem_alloc:f),order(concurrent))]]
  for (int i = 0; i < 64; i++)
    ll++;
}

void
qux (int p)
{
  [[omp::directive (loop, bind(teams),order(concurrent),
    private (p),lastprivate (l),collapse(1),reduction(+:r))]]
  for (l = 0; l < 64; ++l)
    ll++;
}
#pragma omp end declare target

void
baz (int d, int m, int i1, int i2, int p, int *idp, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int ntm)
{
  [[omp::directive (distribute parallel for,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),copyin(t),allocate (p))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (distribute parallel for,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),order(concurrent),allocate (p))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (distribute parallel for simd,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    if (parallel: i2),if(simd: i1),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),nontemporal(ntm),
    safelen(8),simdlen(4),aligned(q: 32),copyin(t),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (distribute parallel for simd,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    if (parallel: i2),if(simd: i1),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),nontemporal(ntm),
    safelen(8),simdlen(4),aligned(q: 32),order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (distribute simd,
    private (p),firstprivate (f),collapse(1),dist_schedule(static, 16),
    safelen(8),simdlen(4),aligned(q: 32),reduction(+:r),if(i1),nontemporal(ntm),
    order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (loop, bind(parallel),order(concurrent),
    private (p),lastprivate (l),collapse(1),reduction(+:r))]]
  for (l = 0; l < 64; ++l)
    ll++;
}

void
bar (int d, int m, int i1, int i2, int i3, int p, int *idp, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int *dd, int ntm,
     const char *msg)
{
  [[omp::directive (nothing)]];
  [[omp::directive (error, at (execution), severity (warning), message (msg))]];
  [[omp::directive (for simd,
    private (p),firstprivate (f),lastprivate (l),linear (ll:1),reduction(+:r),schedule(static, 4),collapse(1),nowait,
    safelen(8),simdlen(4),aligned(q: 32),nontemporal(ntm),if(i1),order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (for,
    private (p),firstprivate (f),lastprivate (l),linear (ll:1),reduction(+:r),schedule(static, 4),collapse(1),nowait,
    order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (simd,
    private (p),lastprivate (l),linear (ll:1),reduction(+:r),collapse(1),safelen(8),simdlen(4),aligned(q: 32),
    nontemporal(ntm),if(i1),order(concurrent))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel for,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),copyin(t),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),linear (ll:1),ordered schedule(static, 4),collapse(1),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel for,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),copyin(t),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),linear (ll:1),schedule(static, 4),collapse(1),order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel for simd,
    private (p),firstprivate (f),if (i2),default(shared),shared(s),copyin(t),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),linear (ll:1),schedule(static, 4),collapse(1),
    safelen(8),simdlen(4),aligned(q: 32),nontemporal(ntm),order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel sections,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),copyin(t),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),allocate (f))]]
  {
    [[using omp:directive (section)]]
    {}
    [[omp::sequence (omp::directive (section))]]
    {}
  }
  [[omp::directive (sections, private (p),firstprivate (f),reduction(+:r),lastprivate (l),allocate (f),nowait)]]
  {
    ;
    [[omp::sequence (sequence (directive (section)))]]
    ;
    [[omp::directive (section)]]
    {}
  }
  [[omp::directive (barrier)]];
  [[using omp:sequence (omp::directive (single, private (p),firstprivate (f),allocate (f),nowait))]]
    ;
  [[omp::sequence (directive (barrier))]];
  [[using omp:sequence (directive (parallel, private (p)),
    omp::directive (single, copyprivate (p),firstprivate (f),allocate (f)))]]
    p = 6;
  [[omp::directive (target parallel,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread)
    nowait depend(inout: dd[0]),allocate (omp_default_mem_alloc:f),in_reduction(+:r2))]]
    ;
  [[omp::directive (target parallel for,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),linear (ll:1),ordered schedule(static, 4),collapse(1),nowait depend(inout: dd[0]),
    allocate (omp_default_mem_alloc:f),in_reduction(+:r2))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[using omp:directive (target parallel for,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),linear (ll:1),schedule(static, 4),collapse(1),nowait depend(inout: dd[0]),order(concurrent),
    allocate (omp_default_mem_alloc:f),in_reduction(+:r2))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (omp::directive (target parallel for simd,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),linear (ll:1),schedule(static, 4),collapse(1),
    safelen(8),simdlen(4),aligned(q: 32),nowait depend(inout: dd[0]),nontemporal(ntm),if (simd: i3),order(concurrent),
    allocate (omp_default_mem_alloc:f),in_reduction(+:r2)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[using omp:sequence (directive (target teams,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),nowait, depend(inout: dd[0]),
    allocate (omp_default_mem_alloc:f) in_reduction(+:r2)))]]
    ;
  [[using omp:sequence (directive (target,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    nowait depend(inout: dd[0]),allocate (omp_default_mem_alloc:f),in_reduction(+:r2)))]]
    ;
  [[omp::sequence (omp::directive (target teams distribute,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),order(concurrent),
    collapse(1),dist_schedule(static, 16),nowait depend(inout: dd[0]),allocate (omp_default_mem_alloc:f),in_reduction(+:r2)))]]
  for (int i = 0; i < 64; i++)
    ;
  [[omp::directive (target teams distribute parallel for,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),nowait depend(inout: dd[0]),order(concurrent),
     allocate (omp_default_mem_alloc:f),in_reduction(+:r2))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (target teams distribute parallel for simd,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),order(concurrent),
    safelen(8),simdlen(4),aligned(q: 32),nowait depend(inout: dd[0]),nontemporal(ntm),if (simd: i3),
    allocate (omp_default_mem_alloc:f),in_reduction(+:r2))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (target teams distribute simd,
    device(d),map (tofrom: m),if (i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),order(concurrent),
    safelen(8),simdlen(4),aligned(q: 32),nowait depend(inout: dd[0]),nontemporal(ntm),
    allocate (omp_default_mem_alloc:f),in_reduction(+:r2))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (target simd,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    safelen(8),simdlen(4),lastprivate (l),linear(ll: 1),aligned(q: 32),reduction(+:r),
    nowait depend(inout: dd[0]),nontemporal(ntm),if(simd:i3),order(concurrent),
    allocate (omp_default_mem_alloc:f),in_reduction(+:r2))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (directive (taskgroup, task_reduction(+:r2), allocate (r2)),
    omp::directive (taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(taskloop: i1),if(simd: i2),final(fi),mergeable,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),in_reduction(+:r2),nontemporal(ntm),
    order(concurrent),allocate (f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[using omp:sequence (omp::directive (taskgroup, task_reduction(+:r), allocate (r)),
    directive (taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(i1),final(fi),mergeable,nogroup,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),in_reduction(+:r),nontemporal(ntm),
    order(concurrent),allocate (f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (taskwait)]];
  [[omp::directive (taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),if(taskloop: i1),final(fi),priority (pp)
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(+:r),if (simd: i3),nontemporal(ntm),
    order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (directive (taskgroup, task_reduction(+:r2), allocate (r2)),
    omp::directive (taskloop
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied, if(taskloop: i1),final(fi),mergeable, priority (pp),
    reduction(default, +:r),in_reduction(+:r2),allocate (f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (directive (taskgroup, task_reduction(+:r2),allocate (r2)),
    omp::directive (task,
    private (p),firstprivate (f),shared (s),default(shared),untied,if(task: i1),final(fi),mergeable,priority (pp),
    in_reduction(+:r2),allocate (f)))]]
    ;
  [[omp::directive (taskyield)]];
  [[omp::directive (target data, if (target data: i1),device(d),map (tofrom: m),use_device_ptr (q),use_device_addr (p))]]
  ;
  [[omp::directive (target enter data, if (target enter data: i1),device(d),map (to: m),depend(inout: dd[0]),nowait)]]
  ;
  [[omp::directive (target exit data, if (target exit data: i1),device(d),map (from: m),depend(inout: dd[0]),nowait)]]
  ;
  [[omp::directive (target update, if (target update: i1),device(d),to (m),depend(inout: dd[0]),nowait)]]
  ;
  [[omp::directive (target update, if (target update: i1),device(d),from (m),depend(inout: dd[0]),nowait)]]
  ;
  [[omp::directive (taskwait)]];
  [[omp::sequence (directive (target, nowait,depend(inout: dd[0]),in_reduction(+:r2)),
    directive (teams distribute,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),allocate (omp_default_mem_alloc: f),order(concurrent)))]]
  for (int i = 0; i < 64; i++)
    ;
  [[omp::directive (teams,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    allocate (omp_default_mem_alloc: f))]]
    ;
  [[omp::sequence (omp::directive (target),
    omp::directive (teams distribute parallel for,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),order(concurrent),allocate (omp_default_mem_alloc: f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[using omp:sequence (directive (target),
    directive (teams distribute parallel for simd,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),order(concurrent),
    safelen(8),simdlen(4),aligned(q: 32),if (simd: i3),nontemporal(ntm),
    allocate (omp_default_mem_alloc: f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (directive (target),
    directive (teams distribute simd,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),order(concurrent),
    safelen(8),simdlen(4),aligned(q: 32),if(i3),nontemporal(ntm),
    allocate (omp_default_mem_alloc: f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (teams distribute parallel for,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),copyin(t),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (teams distribute parallel for,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),order(concurrent),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (teams distribute parallel for simd,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),
    safelen(8),simdlen(4),aligned(q: 32),if (simd: i3),nontemporal(ntm),copyin(t),
    allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (teams distribute parallel for simd,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),
    if (parallel: i2),num_threads (nth),proc_bind(spread),
    lastprivate (l),schedule(static, 4),order(concurrent),
    safelen(8),simdlen(4),aligned(q: 32),if (simd: i3),nontemporal(ntm),
    allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (teams distribute simd,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),dist_schedule(static, 16),order(concurrent),
    safelen(8),simdlen(4),aligned(q: 32),if(i3),nontemporal(ntm),allocate(f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel master,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),reduction(+:r),
    num_threads (nth),proc_bind(spread),copyin(t),allocate (f))]]
    ;
  [[omp::directive (parallel masked,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),reduction(+:r),
    num_threads (nth),proc_bind(spread),copyin(t),allocate (f),filter(d))]]
    ;
  [[omp::directive (parallel,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),reduction(+:r),
    num_threads (nth),proc_bind(spread),copyin(t),allocate (f))]]
    ;
  [[using omp:sequence (directive (taskgroup, task_reduction (+:r2),allocate (r2)),
    omp::directive (master taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied, if(taskloop: i1),final(fi),mergeable, priority (pp),
    reduction(default, +:r),in_reduction(+:r2),allocate (f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[using omp:sequence (directive (taskgroup, task_reduction (+:r2),allocate (r2)),
    omp::directive (masked taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied, if(taskloop: i1),final(fi),mergeable, priority (pp),
    reduction(default, +:r),in_reduction(+:r2),allocate (f),filter(d)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[using omp:directive (master)]];
  [[using omp:directive (masked)]];
  [[using omp:directive (masked,filter(d))]];
  [[omp::sequence (omp::directive (taskgroup task_reduction (+:r2),allocate (r2)),
    directive (master taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(taskloop: i1),if(simd: i2),final(fi),mergeable,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),in_reduction(+:r2),nontemporal(ntm),
    order(concurrent),allocate (f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (omp::directive (taskgroup task_reduction (+:r2),allocate (r2)),
    directive (masked taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(taskloop: i1),if(simd: i2),final(fi),mergeable,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),in_reduction(+:r2),nontemporal(ntm),
    order(concurrent),allocate (f),filter(d)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel master taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(taskloop: i1),final(fi),mergeable,priority (pp),
    reduction(default, +:r),if (parallel: i2),num_threads (nth),proc_bind(spread),copyin(t),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel masked taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(taskloop: i1),final(fi),mergeable,priority (pp),
    reduction(default, +:r),if (parallel: i2),num_threads (nth),proc_bind(spread),copyin(t),allocate (f),filter(d))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel master taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(taskloop: i1),if(simd: i2),final(fi),mergeable,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),nontemporal(ntm),if (parallel: i2),num_threads (nth),proc_bind(spread),copyin(t),
    order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel masked taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),grainsize (g),collapse(1),untied,if(taskloop: i1),if(simd: i2),final(fi),mergeable,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),nontemporal(ntm),if (parallel: i2),num_threads (nth),proc_bind(spread),copyin(t),
    order(concurrent),allocate (f),filter(d))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (directive (taskgroup,task_reduction (+:r2),allocate (r2)),
    directive (master taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied,if(i1),final(fi),mergeable,priority (pp),
    reduction(default, +:r),in_reduction(+:r2)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (directive (taskgroup,task_reduction (+:r2),allocate (r2)),
    directive (masked taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied,if(i1),final(fi),mergeable,priority (pp),
    reduction(default, +:r),in_reduction(+:r2),filter(d)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (omp::directive (taskgroup,task_reduction (+:r2),allocate (r2)),
    omp::directive (master taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied,if(i1),final(fi),mergeable,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),in_reduction(+:r2),nontemporal(ntm),
    order(concurrent),allocate (f)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::sequence (omp::directive (taskgroup,task_reduction (+:r2),allocate (r2)),
    omp::directive (masked taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied,if(i1),final(fi),mergeable,priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),in_reduction(+:r2),nontemporal(ntm),
    order(concurrent),allocate (f),filter(d)))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel master taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied if(i1),final(fi),mergeable priority (pp),
    reduction(default, +:r),num_threads (nth),proc_bind(spread),copyin(t),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel masked taskloop,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied if(i1),final(fi),mergeable priority (pp),
    reduction(default, +:r),num_threads (nth),proc_bind(spread),copyin(t),allocate (f),filter(d))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel master taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied if(i1),final(fi),mergeable priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),nontemporal(ntm),num_threads (nth),proc_bind(spread),copyin(t),
    order(concurrent),allocate (f))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (parallel masked taskloop simd,
    private (p),firstprivate (f),lastprivate (l),shared (s),default(shared),num_tasks (nta),collapse(1),untied if(i1),final(fi),mergeable priority (pp),
    safelen(8),simdlen(4),linear(ll: 1),aligned(q: 32),reduction(default, +:r),nontemporal(ntm),num_threads (nth),proc_bind(spread),copyin(t),
    order(concurrent),allocate (f),filter(d))]]
  for (int i = 0; i < 64; i++)
    ll++;
  [[omp::directive (loop, bind(thread),order(concurrent),
    private (p),lastprivate (l),collapse(1),reduction(+:r))]]
  for (l = 0; l < 64; ++l)
    ll++;
  [[omp::directive (parallel loop,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),copyin(t),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),collapse(1),bind(parallel),order(concurrent),allocate (f))]]
  for (l = 0; l < 64; l++)
    ll++;
  [[omp::directive (parallel loop,
    private (p),firstprivate (f),if (parallel: i2),default(shared),shared(s),copyin(t),reduction(+:r),num_threads (nth),proc_bind(spread),
    lastprivate (l),collapse(1),allocate (f))]]
  for (l = 0; l < 64; l++)
    ll++;
  [[omp::directive (teams loop,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),lastprivate (l),bind(teams),allocate (f))]]
  for (l = 0; l < 64; ++l)
    ;
  [[omp::directive (teams loop,
    private(p),firstprivate (f),shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),
    collapse(1),lastprivate (l),order(concurrent),allocate (f))]]
  for (l = 0; l < 64; ++l)
    ;
  [[omp::directive (target parallel loop,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    nowait depend(inout: dd[0]),lastprivate (l),bind(parallel),order(concurrent),collapse(1),
    allocate (omp_default_mem_alloc: f),in_reduction(+:r2))]]
  for (l = 0; l < 64; ++l)
    ;
  [[omp::directive (target parallel loop,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    if (parallel: i2),default(shared),shared(s),reduction(+:r),num_threads (nth),proc_bind(spread),
    nowait depend(inout: dd[0]),lastprivate (l),order(concurrent),collapse(1),
    allocate (omp_default_mem_alloc: f),in_reduction(+:r2))]]
  for (l = 0; l < 64; ++l)
    ;
  [[omp::directive (target teams loop,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),nowait,depend(inout: dd[0]),
    lastprivate (l),bind(teams),collapse(1),
    allocate (omp_default_mem_alloc: f),in_reduction(+:r2))]]
  for (l = 0; l < 64; ++l)
    ;
  [[omp::directive (target teams loop,
    device(d),map (tofrom: m),if (target: i1),private (p),firstprivate (f),defaultmap(tofrom: scalar),is_device_ptr (idp),
    shared(s),default(shared),reduction(+:r),num_teams(nte),thread_limit(tl),nowait,depend(inout: dd[0]),
    lastprivate (l),order(concurrent),collapse(1)
    allocate (omp_default_mem_alloc: f),in_reduction(+:r2))]]
  for (l = 0; l < 64; ++l)
    ;
  [[omp::directive (critical)]] {
  }
  [[omp::directive (critical (foobar),hint(omp_sync_hint_none))]]
  ;
  [[using omp:directive (taskwait, depend (inout: dd[0]))]]
  ;
  [[omp::directive (taskgroup, task_reduction(+:r2),allocate (r2))]]
  ;
  [[omp::directive (atomic, update,seq_cst,hint(omp_sync_hint_none))]]
  p++;
  [[omp::directive (atomic, read, hint(omp_sync_hint_none),relaxed)]]
  f = p;
  [[omp::directive (atomic,write, release hint(omp_sync_hint_none))]]
  p = f;
  [[omp::directive (flush)]]
  ;
  [[omp::directive (flush, acq_rel)]]
  ;
  [[omp::directive (flush, acquire)]]
  ;
  [[omp::directive (flush, release)]]
  ;
  [[omp::directive (flush, seq_cst)]]
  ;
  [[omp::directive (flush (p, f))]]
  ;
  [[omp::directive (simd,
    private (p),lastprivate (l),linear (ll:1),reduction(+:r),collapse(1),safelen(8),simdlen(4),aligned(q: 32),
    nontemporal(ntm),if(i1))]]
  for (int i = 0; i < 64; i++)
    [[omp::directive (ordered, simd)]]
      ll++;
  [[omp::directive (for,
    private (p),firstprivate (f),lastprivate (l),linear (ll:1),reduction(+:r),schedule(static, 4),collapse(1),nowait,
    ordered, allocate (f))]]
  for (int i = 0; i < 64; i++)
    [[omp::directive (ordered, threads)]]
      ll++;
  [[omp::directive(for, ordered (1))]]
  for (l = 0; l < 64; l++)
    {
      [[omp::directive(ordered, depend (sink: l - 1))]];
      [[omp::directive(ordered, depend (source))]];
    }
  extern omp_depend_t depobj;
  [[omp::directive (depobj(depobj),depend(in : dd[0]))]];
  [[omp::directive (parallel)]] {
    if (p) {
      [[omp::directive (cancel, parallel)]];
    } else {
      [[omp::directive (cancellation point, parallel)]];
    }
  }
  [[omp::directive (scope, private (p), reduction(+:r), nowait)]]
    ;
  [[using omp:directive (scope, private (p), reduction(task, +:r))]]
    ;
  extern int t2;
  [[omp::directive (threadprivate (t2))]];
  extern int t2;
  [[omp::directive (declare reduction (dr: int: omp_out += omp_in),initializer (omp_priv = 0))]]
  ;
}

void corge1 ();

void
corge ()
{
  [[omp::directive (declare variant (corge1),match (construct={parallel,for}))]]
  extern void corge2 ();
  [[omp::sequence (directive (parallel), directive (for))]]
  for (int i = 0; i < 5; i++)
    corge2 ();
  [[omp::directive (declare simd, simdlen(4),linear(l),aligned(p:4),uniform(p),inbranch),
    omp::directive (declare simd,simdlen(8),notinbranch)]]
  extern int corge3 (int l, int *p);
  [[using omp:directive (declare simd, simdlen(4),linear(l),aligned(p:4),uniform(p),inbranch),
    directive (declare simd, simdlen(8),notinbranch)]]
  extern int corge4 (int l, int *p);
  [[omp::sequence (directive (declare simd, simdlen(4),linear(l),aligned(p:4),uniform(p),inbranch),
    omp::directive (declare simd, simdlen(8),notinbranch))]]
  extern int corge5 (int l, int *p);
  [[omp::directive (declare target)]];
  extern void corge6 ();
  [[omp::directive (end declare target)]];
}

int
garply (int a, int *c, int *d, int *e, int *f)
{
  int i;
  [[omp::directive (simd, reduction (inscan, +: a))]]
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      [[omp::directive (scan, exclusive (a))]]
      a += c[i];
    }
  [[omp::directive (simd, reduction (inscan, +: a))]]
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      [[using omp : sequence (sequence (directive (scan inclusive (a))))]]
      d[i] = a;
    }
  return a;
}
