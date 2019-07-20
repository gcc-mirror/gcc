/* { dg-do compile } */
/* { dg-additional-options "-std=c99" { target c } } */

int t;
#pragma omp threadprivate (t)

#pragma omp declare target
int f, l, ll, r, r2;

void
foo (int d, int m, int i1, int i2, int p, int *idp, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int ntm)
{
  #pragma omp distribute parallel for \
    private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute parallel for simd \
    private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) if(simd: i1) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) nontemporal(ntm) \
    safelen(8) simdlen(4) aligned(q: 32) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute simd \
    private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) \
    safelen(8) simdlen(4) aligned(q: 32) reduction(+:r) if(i1) nontemporal(ntm) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
}

void
qux (int p)
{
  #pragma omp loop bind(teams) order(concurrent) \
    private (p) lastprivate (l) collapse(1) reduction(+:r)
  for (l = 0; l < 64; ++l)
    ll++;
}
#pragma omp end declare target

void
baz (int d, int m, int i1, int i2, int p, int *idp, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int ntm)
{
  #pragma omp distribute parallel for \
    private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) copyin(t) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute parallel for simd \
    private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) if(simd: i1) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) nontemporal(ntm) \
    safelen(8) simdlen(4) aligned(q: 32) copyin(t) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp distribute simd \
    private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) \
    safelen(8) simdlen(4) aligned(q: 32) reduction(+:r) if(i1) nontemporal(ntm) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp loop bind(parallel) order(concurrent) \
    private (p) lastprivate (l) collapse(1) reduction(+:r)
  for (l = 0; l < 64; ++l)
    ll++;
}

void
bar (int d, int m, int i1, int i2, int i3, int p, int *idp, int s,
     int nte, int tl, int nth, int g, int nta, int fi, int pp, int *q, int *dd, int ntm)
{
  #pragma omp for simd \
    private (p) firstprivate (f) lastprivate (l) linear (ll:1) reduction(+:r) schedule(static, 4) collapse(1) nowait \
    safelen(8) simdlen(4) aligned(q: 32) nontemporal(ntm) if(i1) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel for \
    private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) linear (ll:1) ordered schedule(static, 4) collapse(1)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel for \
    private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel for simd \
    private (p) firstprivate (f) if (i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) \
    safelen(8) simdlen(4) aligned(q: 32) nontemporal(ntm) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel sections \
    private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l)
  {
    #pragma omp section
    {}
    #pragma omp section
    {}
  }
  #pragma omp target parallel \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    nowait depend(inout: dd[0])
    ;
  #pragma omp target parallel for \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) linear (ll:1) ordered schedule(static, 4) collapse(1) nowait depend(inout: dd[0])
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target parallel for \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) nowait depend(inout: dd[0]) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target parallel for simd \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) \
    safelen(8) simdlen(4) aligned(q: 32) nowait depend(inout: dd[0]) nontemporal(ntm) if (simd: i3) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target teams \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) nowait depend(inout: dd[0])
    ;
  #pragma omp target teams distribute \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) nowait depend(inout: dd[0])
  for (int i = 0; i < 64; i++)
    ;
  #pragma omp target teams distribute parallel for \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) nowait depend(inout: dd[0]) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target teams distribute parallel for simd \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) order(concurrent) \
    safelen(8) simdlen(4) aligned(q: 32) nowait depend(inout: dd[0]) nontemporal(ntm) if (simd: i3)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target teams distribute simd \
    device(d) map (tofrom: m) if (i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) order(concurrent) \
    safelen(8) simdlen(4) aligned(q: 32) nowait depend(inout: dd[0]) nontemporal(ntm)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target simd \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    safelen(8) simdlen(4) lastprivate (l) linear(ll: 1) aligned(q: 32) reduction(+:r) \
     nowait depend(inout: dd[0]) nontemporal(ntm) if(simd:i3) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup task_reduction(+:r2)
  #pragma omp taskloop simd \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) \
    safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup task_reduction(+:r)
  #pragma omp taskloop simd \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(i1) final(fi) mergeable nogroup priority (pp) \
    safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) in_reduction(+:r) nontemporal(ntm) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskwait
  #pragma omp taskloop simd \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) if(taskloop: i1) final(fi) priority (pp) \
    safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(+:r) if (simd: i3) nontemporal(ntm) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target nowait depend(inout: dd[0])
  #pragma omp teams distribute \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16)
  for (int i = 0; i < 64; i++)
    ;
  #pragma omp target
  #pragma omp teams distribute parallel for \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target
  #pragma omp teams distribute parallel for simd \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) order(concurrent) \
    safelen(8) simdlen(4) aligned(q: 32) if (simd: i3) nontemporal(ntm)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp target
  #pragma omp teams distribute simd \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) order(concurrent) \
    safelen(8) simdlen(4) aligned(q: 32) if(i3) nontemporal(ntm)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute parallel for \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) order(concurrent) \
    if (parallel: i2) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) copyin(t)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute parallel for simd \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) \
    if (parallel: i2) num_threads (nth) proc_bind(spread) \
    lastprivate (l) schedule(static, 4) order(concurrent) \
    safelen(8) simdlen(4) aligned(q: 32) if (simd: i3) nontemporal(ntm) copyin(t)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp teams distribute simd \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) dist_schedule(static, 16) order(concurrent) \
    safelen(8) simdlen(4) aligned(q: 32) if(i3) nontemporal(ntm)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master \
    private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) reduction(+:r) \
    num_threads (nth) proc_bind(spread) copyin(t)
    ;
  #pragma omp taskgroup task_reduction (+:r2)
  #pragma omp master taskloop \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) final(fi) mergeable priority (pp) \
    reduction(default, +:r) in_reduction(+:r2)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup task_reduction (+:r2)
  #pragma omp master taskloop simd \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) \
    safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) final(fi) mergeable priority (pp) \
    reduction(default, +:r) if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop simd \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) \
    safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) nontemporal(ntm) if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup task_reduction (+:r2)
  #pragma omp master taskloop \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied if(i1) final(fi) mergeable priority (pp) \
    reduction(default, +:r) in_reduction(+:r2)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp taskgroup task_reduction (+:r2)
  #pragma omp master taskloop simd \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied if(i1) final(fi) mergeable priority (pp) \
    safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied if(i1) final(fi) mergeable priority (pp) \
    reduction(default, +:r) num_threads (nth) proc_bind(spread) copyin(t)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp parallel master taskloop simd \
    private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied if(i1) final(fi) mergeable priority (pp) \
    safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) nontemporal(ntm) num_threads (nth) proc_bind(spread) copyin(t) \
    order(concurrent)
  for (int i = 0; i < 64; i++)
    ll++;
  #pragma omp loop bind(thread) order(concurrent) \
    private (p) lastprivate (l) collapse(1) reduction(+:r)
  for (l = 0; l < 64; ++l)
    ll++;
  #pragma omp parallel loop \
    private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) collapse(1) bind(parallel) order(concurrent)
  for (l = 0; l < 64; l++)
    ll++;
  #pragma omp parallel loop \
    private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) proc_bind(spread) \
    lastprivate (l) collapse(1)
  for (l = 0; l < 64; l++)
    ll++;
  #pragma omp teams loop \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) lastprivate (l) bind(teams)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp teams loop \
    private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) \
    collapse(1) lastprivate (l) order(concurrent)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target parallel loop \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    nowait depend(inout: dd[0]) lastprivate (l) bind(parallel) order(concurrent) collapse(1)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target parallel loop \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) \
    nowait depend(inout: dd[0]) lastprivate (l) order(concurrent) collapse(1)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target teams loop \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) nowait depend(inout: dd[0]) \
    lastprivate (l) bind(teams) collapse(1)
  for (l = 0; l < 64; ++l)
    ;
  #pragma omp target teams loop \
    device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) \
    shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) nowait depend(inout: dd[0]) \
    lastprivate (l) order(concurrent) collapse(1)
  for (l = 0; l < 64; ++l)
    ;
}
