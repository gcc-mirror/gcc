module m
  implicit none
  integer :: t
  !$omp threadprivate (t)
  integer :: f, l, ll, r, r2
  !$omp declare target to(f, l, ll, r, r2)
end module m

subroutine foo(fi, p, pp, g, s, nta, nth, ntm, i1, i2, i3, q)
  use m
  implicit none
  integer, value :: p, pp, g, s, nta, nth, ntm
  logical, value :: fi, i1, i2, i3
  integer, pointer :: q(:)
  integer :: i

  !$omp taskgroup task_reduction(+:r2) !allocate (r2)
  !$omp taskloop simd &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) &
  !$omp& if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp& safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) &
  !$omp& order(concurrent) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction(+:r) !allocate (r)
  !$omp taskloop simd &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) &
  !$omp& collapse(1) untied if(i1) final(fi) mergeable nogroup priority (pp) &
  !$omp& safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) in_reduction(+:r) nontemporal(ntm) &
  !$omp& order(concurrent) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp taskwait

  !$omp taskloop simd &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) &
  !$omp& collapse(1) if(taskloop: i1) final(fi) priority (pp) &
  !$omp& safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(+:r) if (simd: i3) nontemporal(ntm) &
  !$omp& order(concurrent) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) !allocate (r2)
  !$omp master taskloop &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) &
  !$omp& collapse(1) untied if(taskloop: i1) final(fi) mergeable priority (pp) &
  !$omp& reduction(default, +:r) in_reduction(+:r2) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) !allocate (r2)
  !$omp master taskloop simd &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) &
  !$omp& collapse(1) untied if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp& safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) &
  !$omp& order(concurrent) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp parallel master taskloop &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) &
  !$omp& collapse(1) untied if(taskloop: i1) final(fi) mergeable priority (pp) &
  !$omp& reduction(default, +:r) if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp parallel master taskloop simd &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) &
  !$omp& untied if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
  !$omp& safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) nontemporal(ntm) &
  !$omp& if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t) &
  !$omp& order(concurrent) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp taskgroup task_reduction (+:r2) !allocate (r2)
  !$omp master taskloop &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) &
  !$omp& collapse(1) untied if(i1) final(fi) mergeable priority (pp) &
  !$omp& reduction(default, +:r) in_reduction(+:r2)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2) !allocate (r2)
  !$omp master taskloop simd &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) &
  !$omp& collapse(1) untied if(i1) final(fi) mergeable priority (pp) &
  !$omp& safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) &
  !$omp& order(concurrent) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
  !$omp end taskgroup

  !$omp parallel master taskloop &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) &
  !$omp& collapse(1) untied if(i1) final(fi) mergeable priority (pp) &
  !$omp& reduction(default, +:r) num_threads (nth) proc_bind(spread) copyin(t) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do

  !$omp parallel master taskloop simd &
  !$omp& private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) num_tasks (nta) &
  !$omp& collapse(1) untied if(i1) final(fi) mergeable priority (pp) &
  !$omp& safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) &
  !$omp& nontemporal(ntm) num_threads (nth) proc_bind(spread) copyin(t) &
  !$omp& order(concurrent) !allocate (f)
  do i = 1, 64
    ll = ll + 1
  end do
end
