! { dg-do compile }

module m
  use iso_c_binding, only: c_intptr_t
  implicit none (external, type)

  integer(c_intptr_t), parameter :: &
    omp_null_allocator = 0,         &
    omp_default_mem_alloc = 1,      &
    omp_large_cap_mem_alloc = 2,    &
    omp_const_mem_alloc = 3,        &
    omp_high_bw_mem_alloc = 4,      &
    omp_low_lat_mem_alloc = 5,      &
    omp_cgroup_mem_alloc = 6,       &
    omp_pteam_mem_alloc = 7,        &
    omp_thread_mem_alloc = 8

  integer, parameter :: &
    omp_allocator_handle_kind = c_intptr_t

  integer :: t
  !$omp threadprivate (t)

  integer :: f, l, ll, r, r2
  !$omp declare target (f, l, ll, r, r2)

contains

subroutine foo (d, m, i1, i2, i3, p, idp, s, nte, tl, nth, g, nta, fi, pp, q, dd, ntm)
  !$omp declare target (foo)
  integer :: d, m, p, idp, s, nte, tl, nth, g, nta, pp, q, dd, ntm
  logical :: i1, i2, i3, fi
  pointer :: q
  integer :: i

  !$omp distribute parallel do &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) order(concurrent) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp distribute parallel do simd &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) if(simd: i1) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) nontemporal(ntm) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) order(concurrent) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp distribute simd &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) reduction(+:r) if(i1) nontemporal(ntm) &
  !$omp&  order(concurrent) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll +1
  end do
end

subroutine qux (p)
  !$omp declare target (qux)
  integer, value :: p

  !$omp loop bind(teams) order(concurrent) &
  !$omp&  private (p) lastprivate (l) collapse(1) reduction(+:r)
  do l = 1, 64
    ll = ll + 1
  end do
end

subroutine baz (d, m, i1, i2, i3, p, idp, s, nte, tl, nth, g, nta, fi, pp, q, dd, ntm)
  integer :: d, m, p, idp, s, nte, tl, nth, g, nta, pp, q, dd, ntm
  logical :: i1, i2, i3, fi
  pointer :: q
  integer :: i
  !$omp distribute parallel do &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) copyin(t) &
  !$omp&  allocate (p)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp distribute parallel do &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) order(concurrent) &
  !$omp&  allocate (p)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp distribute parallel do simd &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) if(simd: i1) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) nontemporal(ntm) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) copyin(t) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp distribute parallel do simd &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) if(simd: i1) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) nontemporal(ntm) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp distribute simd &
  !$omp&  private (p) firstprivate (f) collapse(1) dist_schedule(static, 16) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) reduction(+:r) if(i1) nontemporal(ntm) &
  !$omp&  order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp loop bind(parallel) order(concurrent) &
  !$omp&  private (p) lastprivate (l) collapse(1) reduction(+:r)
  do l = 1, 64
    ll = ll + 1
  end do
end

subroutine bar (d, m, i1, i2, i3, p, idp, s, nte, tl, nth, g, nta, fi, pp, q, dd, ntm)
  integer :: d, m, p, idp, s, nte, tl, nth, g, nta, pp, q, dd(0:5), ntm
  logical :: i1, i2, i3, fi
  pointer :: q
  integer :: i

  !$omp do simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) linear (ll:1) reduction(+:r) schedule(static, 4) collapse(1) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) nontemporal(ntm) if(i1) order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end do simd nowait

  !$omp parallel do &
  !$omp&  private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) &
  !$omp&  proc_bind(spread) lastprivate (l) linear (ll:1) ordered schedule(static, 4) collapse(1) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp parallel do &
  !$omp&  private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) &
  !$omp&  proc_bind(spread) lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp parallel do simd &
  !$omp&  private (p) firstprivate (f) if (i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) &
  !$omp&  proc_bind(spread) lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) nontemporal(ntm) order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp parallel sections &
  !$omp&  private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) &
  !$omp&  proc_bind(spread) lastprivate (l) &
  !$omp&  allocate (f)
    !$omp section
      block; end block
    !$omp section
      block; end block
  !$omp end parallel sections

  !$omp target parallel &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  depend(inout: dd(0)) in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  !$omp end target parallel nowait

  !$omp target parallel do &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) linear (ll:1) ordered schedule(static, 4) collapse(1) depend(inout: dd(0)) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end target parallel do nowait

  !$omp target parallel do &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) depend(inout: dd(0)) order(concurrent) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end target parallel do nowait

  !$omp target parallel do simd &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  if (parallel: i2) default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) linear (ll:1) schedule(static, 4) collapse(1) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) depend(inout: dd(0)) nontemporal(ntm) if (simd: i3) order(concurrent) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end target parallel do simd nowait

  !$omp target teams &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  shared(s) default(shared) reduction(+:r) num_teams(nte - 1:nte) thread_limit(tl) depend(inout: dd(0)) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  !$omp end target teams nowait

  !$omp target teams distribute &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) order(concurrent) &
  !$omp&  collapse(1) dist_schedule(static, 16) depend(inout: dd(0)) in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
  end do
  !$omp end target teams distribute nowait

  !$omp target teams distribute parallel do &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) depend(inout: dd(0)) order(concurrent) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end target teams distribute parallel do nowait

  !$omp target teams distribute parallel do simd &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) order(concurrent) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) depend(inout: dd(0)) nontemporal(ntm) if (simd: i3) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end target teams distribute parallel do simd nowait

  !$omp target teams distribute simd &
  !$omp&  device(d) map (tofrom: m) if (i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) order(concurrent) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) depend(inout: dd(0)) nontemporal(ntm) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end target teams distribute simd nowait

  !$omp target simd &
  !$omp&  device(d) map (tofrom: m) if (target: i1) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  safelen(8) simdlen(4) lastprivate (l) linear(ll: 1) aligned(q: 32) reduction(+:r) &
  !$omp&  depend(inout: dd(0)) nontemporal(ntm) if(simd:i3) order(concurrent) &
  !$omp&  in_reduction(+:r2) &
  !$omp&  allocate (omp_default_mem_alloc:f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end target simd nowait

  !$omp taskgroup task_reduction(+:r2) &
  !$omp&  allocate (r2)
  !$omp taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) &
  !$omp&  order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction(+:r) &
  !$omp&  allocate (r)
  !$omp taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(i1) &
  !$omp&  final(fi) mergeable nogroup priority (pp) &
  !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) in_reduction(+:r) nontemporal(ntm) &
  !$omp&  order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp taskwait
  !$omp taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) if(taskloop: i1) &
  !$omp&  final(fi) priority (pp) safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(+:r) if (simd: i3) nontemporal(ntm) &
  !$omp&  order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp target depend(inout: dd(0)) in_reduction(+:r2)
  !$omp teams distribute &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) order(concurrent) &
  !$omp&  allocate (omp_default_mem_alloc: f)
  do i = 1, 64
  end do
  !$omp end target nowait

  !$omp target
  !$omp teams distribute parallel do &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) order(concurrent) &
  !$omp&  allocate (omp_default_mem_alloc: f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end target

  !$omp target
  !$omp teams distribute parallel do simd &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) order(concurrent) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) if (simd: i3) nontemporal(ntm) &
  !$omp&  allocate (omp_default_mem_alloc: f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end target

  !$omp target
  !$omp teams distribute simd &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) order(concurrent) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) if(i3) nontemporal(ntm) &
  !$omp&  allocate (omp_default_mem_alloc: f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end target

  !$omp teams distribute parallel do &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) copyin(t) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp teams distribute parallel do &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) order(concurrent) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp teams distribute parallel do simd &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) if (simd: i3) nontemporal(ntm) copyin(t) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp teams distribute parallel do simd &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) &
  !$omp&  if (parallel: i2) num_threads (nth) proc_bind(spread) &
  !$omp&  lastprivate (l) schedule(static, 4) order(concurrent) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) if (simd: i3) nontemporal(ntm) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp teams distribute simd &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) &
  !$omp&  collapse(1) dist_schedule(static, 16) order(concurrent) &
  !$omp&  safelen(8) simdlen(4) aligned(q: 32) if(i3) nontemporal(ntm) &
  !$omp&  allocate(f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp parallel master &
  !$omp&  private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) reduction(+:r) &
  !$omp&  num_threads (nth) proc_bind(spread) copyin(t) &
  !$omp&  allocate (f)
  !$omp end parallel master

  !$omp parallel masked &
  !$omp&  private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) reduction(+:r) &
  !$omp&  num_threads (nth) proc_bind(spread) copyin(t) filter (d) &
  !$omp&  allocate (f)
  !$omp end parallel masked

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp master taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) final(fi) mergeable priority (pp) &
  !$omp&  reduction(default, +:r) in_reduction(+:r2) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp masked taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) final(fi) mergeable priority (pp) reduction(default, +:r) in_reduction(+:r2) filter (d) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp master taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) &
  !$omp&  order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp masked taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) &
  !$omp&  order(concurrent) filter (d) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp parallel master taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) final(fi) mergeable priority (pp) &
  !$omp&  reduction(default, +:r) if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp parallel masked taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) final(fi) mergeable priority (pp) &
  !$omp&  reduction(default, +:r) if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t) filter (d) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp parallel master taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) nontemporal(ntm) if (parallel: i2) &
  !$omp&  num_threads (nth) proc_bind(spread) copyin(t) order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp parallel masked taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied &
  !$omp&  if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) nontemporal(ntm) if (parallel: i2) &
  !$omp&  num_threads (nth) proc_bind(spread) copyin(t) order(concurrent) filter (d) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp master taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) &
  !$omp&  untied if(i1) final(fi) mergeable priority (pp) reduction(default, +:r) in_reduction(+:r2)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp masked taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) &
  !$omp&  untied if(i1) final(fi) mergeable priority (pp) reduction(default, +:r) in_reduction(+:r2) filter (d)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp master taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied if(i1) &
  !$omp&  final(fi) mergeable priority (pp) safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) &
  !$omp&  in_reduction(+:r2) nontemporal(ntm) order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) &
  !$omp&  allocate (r2)
  !$omp masked taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied &
  !$omp&  if(i1) final(fi) mergeable priority (pp) safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) &
  !$omp&  in_reduction(+:r2) nontemporal(ntm) order(concurrent) filter (d) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do
  !$omp end taskgroup

  !$omp parallel master taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied &
  !$omp&  if(i1) final(fi) mergeable priority (pp) reduction(default, +:r) num_threads (nth) proc_bind(spread) copyin(t) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp parallel masked taskloop &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied &
  !$omp&  if(i1) final(fi) mergeable priority (pp) reduction(default, +:r) num_threads (nth) proc_bind(spread) &
  !$omp&  copyin(t) filter (d) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp parallel master taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied &
  !$omp&  if(i1) final(fi) mergeable priority (pp) safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) &
  !$omp&  nontemporal(ntm) num_threads (nth) proc_bind(spread)copyin(t) order(concurrent) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp parallel masked taskloop simd &
  !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) collapse(1) untied if(i1) &
  !$omp&  final(fi) mergeable priority (pp) safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) &
  !$omp&  nontemporal(ntm) num_threads (nth) proc_bind(spread) copyin(t) order(concurrent) filter (d) &
  !$omp&  allocate (f)
  do i = 1, 64
    ll = ll +1
  end do

  !$omp loop bind(thread) order(concurrent) &
  !$omp&  private (p) lastprivate (l) collapse(1) reduction(+:r)
  do l = 1, 64
    ll = ll + 1
  end do

  !$omp parallel loop &
  !$omp&  private (p) firstprivate (f) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) &
  !$omp&  proc_bind(spread) lastprivate (l) collapse(1) bind(parallel) order(concurrent) if (parallel: i2) &
  !$omp&  allocate (f)
  do l = 1, 64
    ll = ll + 1
  end do

  !$omp parallel loop &
  !$omp&  private (p) firstprivate (f) default(shared) shared(s) copyin(t) reduction(+:r) num_threads (nth) &
  !$omp&  proc_bind(spread) lastprivate (l) collapse(1) if (parallel: i2) &
  !$omp&  allocate (f)
  do l = 1, 64
    ll = ll + 1
  end do

  !$omp teams loop &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) &
  !$omp&  collapse(1) lastprivate (l) bind(teams) &
  !$omp&  allocate (f)
  do l = 1, 64
  end do

  !$omp teams loop &
  !$omp&  private(p) firstprivate (f) shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) &
  !$omp&  collapse(1) lastprivate (l) order(concurrent) &
  !$omp&  allocate (f)
  do l = 1, 64
  end do

  !$omp target parallel loop &
  !$omp&  device(d) map (tofrom: m) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  default(shared) shared(s) reduction(+:r) num_threads (nth) proc_bind(spread) &
  !$omp&  depend(inout: dd(0)) lastprivate (l) order(concurrent) collapse(1) in_reduction(+:r2) &
  !$omp&  if (target: i1) if (parallel: i2) &
  !$omp&  allocate (omp_default_mem_alloc: f)
  do l = 1, 64
  end do
  !$omp end target parallel loop nowait

  !$omp target teams loop &
  !$omp&  device(d) map (tofrom: m) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  shared(s) default(shared) reduction(+:r) num_teams(nte-1:nte) thread_limit(tl) depend(inout: dd(0)) &
  !$omp&  lastprivate (l) bind(teams) collapse(1) in_reduction(+:r2) if (target: i1) &
  !$omp&  allocate (omp_default_mem_alloc: f)
  do l = 1, 64
  end do
  !$omp end target teams loop nowait

  !$omp target teams loop &
  !$omp&  device(d) map (tofrom: m) private (p) firstprivate (f) defaultmap(tofrom: scalar) is_device_ptr (idp) &
  !$omp&  shared(s) default(shared) reduction(+:r) num_teams(nte) thread_limit(tl) depend(inout: dd(0)) &
  !$omp&  lastprivate (l) order(concurrent) collapse(1) in_reduction(+:r2) if (target: i1) &
  !$omp&  allocate (omp_default_mem_alloc: f)
  do l = 1, 64
  end do
  !$omp end target teams loop nowait

end
end module
