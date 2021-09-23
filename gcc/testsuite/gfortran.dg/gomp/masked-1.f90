! { dg-additional-options "-ffree-line-length-none" }
subroutine foo (x, a)
  implicit none
  integer, value :: x
  integer, contiguous :: a(0:)
  external :: bar
  integer :: i

  !$omp masked
   call bar ()
  !$omp end masked

  !$omp masked filter (0)
   call bar ()
  !$omp end masked

  !$omp masked filter (7)
   call bar ()
  !$omp end masked

  !$omp masked filter (x)
   call bar ()
  !$omp end masked

  !$omp masked taskloop simd filter (x) grainsize (12) simdlen (4)
    do i = 0, 127
      a(i) = i
    end do
  !$omp end masked taskloop simd

  !$omp parallel masked filter (x) firstprivate (x)
    call bar ()
  !$omp end parallel masked

  !$omp masked
    !$omp masked filter (0)
      !$omp masked filter (x)
      !$omp end masked
    !$omp end masked
  !$omp end masked
end

subroutine foobar (d, f, fi, p, s, g, i1, i2, l, ll, nth, ntm, pp, q, r, r2)
  implicit none (type, external)
  logical :: i1, i2, fi
  integer :: i, d, f, p, s, g, l, ll, nth, ntm, pp, q, r, r2
  allocatable :: q
  integer, save :: t
  !$omp threadprivate (t)

  !$omp parallel masked &
  !$omp&  private (p) firstprivate (f) if (parallel: i2) default(shared) shared(s) reduction(+:r) &
  !$omp&  num_threads (nth) proc_bind(spread) copyin(t) filter (d)  ! allocate (f)
    !
  !$omp end parallel masked

  !$omp taskgroup task_reduction (+:r2)  ! allocate (r2)
    !$omp masked taskloop &
    !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) final(fi) mergeable priority (pp) &
    !$omp&  reduction(default, +:r) in_reduction(+:r2) filter (d)  ! allocate (f)
    do i = 0, 63
      ll = ll + 1
    end do
    !$omp end masked taskloop
  !$omp end taskgroup

  !$omp taskgroup task_reduction (+:r2)  ! allocate (r2)
    !$omp masked taskloop simd &
    !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
    !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) in_reduction(+:r2) nontemporal(ntm) &
    !$omp&  order(concurrent) filter (d)  !  allocate (f)
    do i = 0, 63
      ll = ll + 1
    end do
    !$omp end masked taskloop simd
  !$omp end taskgroup

  !$omp parallel masked taskloop &
    !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) final(fi) mergeable priority (pp) &
    !$omp&  reduction(default, +:r) if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t) filter (d)  ! allocate (f)
    do i = 0, 63
      ll = ll + 1
    end do
  !$omp end parallel masked taskloop

  !$omp parallel masked taskloop simd &
    !$omp&  private (p) firstprivate (f) lastprivate (l) shared (s) default(shared) grainsize (g) collapse(1) untied if(taskloop: i1) if(simd: i2) final(fi) mergeable priority (pp) &
    !$omp&  safelen(8) simdlen(4) linear(ll: 1) aligned(q: 32) reduction(default, +:r) nontemporal(ntm) if (parallel: i2) num_threads (nth) proc_bind(spread) copyin(t) &
    !$omp&  order(concurrent) filter (d)  ! allocate (f)
    do i = 0, 63
      ll = ll + 1
    end do
  !$omp end parallel masked taskloop simd
end subroutine
