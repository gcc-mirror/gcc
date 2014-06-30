! { dg-do compile }
! { dg-options "-fopenmp" }

module target1
  interface
    subroutine dosomething (a, n, m)
      integer :: a (:), n, m
      !$omp declare target
    end subroutine dosomething
  end interface
contains
  subroutine foo (n, o, p, q, r, pp)
    integer :: n, o, p, q, r, s, i, j
    integer :: a (2:o)
    integer, pointer :: pp
  !$omp target data device (n + 1) if (n .ne. 6) map (tofrom: n, r)
    !$omp target device (n + 1) if (n .ne. 6) map (from: n) map (alloc: a(2:o))
      call dosomething (a, n, 0)
    !$omp end target
    !$omp target teams device (n + 1) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r)
      r = r + 1
      p = q
      call dosomething (a, n, p + q)
    !$omp end target teams
    !$omp target teams distribute device (n + 1) num_teams (n + 4) collapse (2) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp target teams distribute device (n + 1) num_teams (n + 4) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp end target teams distribute
    !$omp target teams distribute parallel do device (n + 1) num_teams (n + 4) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) collapse (2) &
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s) &
    !$omp & ordered schedule (static, 8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
	  !$omp ordered
	    p = q
	  !$omp end ordered
	  s = i * 10 + j
        end do
      end do
    !$omp target teams distribute parallel do device (n + 1) num_teams (n + 4) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) ordered schedule (static, 8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
        !$omp ordered
          p = q
        !$omp end ordered
	s = i * 10
      end do
    !$omp end target teams distribute parallel do
    !$omp target teams distribute parallel do simd device (n + 1) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) collapse (2) &
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s) &
    !$omp & schedule (static, 8) num_teams (n + 4) safelen(8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp target teams distribute parallel do simd device (n + 1) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) schedule (static, 8) &
    !$omp & num_teams (n + 4) safelen(16) linear(i:1) aligned (pp:4)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end target teams distribute parallel do simd
    !$omp target teams distribute simd device (n + 1) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) collapse (2) &
    !$omp & lastprivate (s) num_teams (n + 4) safelen(8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp target teams distribute simd device (n + 1) &
    !$omp & if (n .ne. 6)map (from: n) map (alloc: a(2:o)) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) lastprivate (s) &
    !$omp & num_teams (n + 4) safelen(16) linear(i:1) aligned (pp:4)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end target teams distribute simd
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams num_teams (n + 4) thread_limit (n * 2) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r)
      r = r + 1
      p = q
      call dosomething (a, n, p + q)
    !$omp end teams
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute num_teams (n + 4) collapse (2) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute num_teams (n + 4) default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp end teams distribute
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute parallel do num_teams (n + 4) &
    !$omp & if (n .ne. 6) default(shared) ordered schedule (static, 8) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) collapse (2) &
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
	  !$omp ordered
	    p = q
	  !$omp end ordered
	  s = i * 10 + j
        end do
      end do
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute parallel do num_teams (n + 4)if(n.ne.6)default(shared)&
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) ordered schedule (static, 8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
        !$omp ordered
          p = q
        !$omp end ordered
	s = i * 10
      end do
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute parallel do simd if(n.ne.6)default(shared)&
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) collapse (2) &
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s) &
    !$omp & schedule (static, 8) num_teams (n + 4) safelen(8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute parallel do simd if (n .ne. 6)default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) schedule (static, 8) &
    !$omp & num_teams (n + 4) safelen(16) linear(i:1) aligned (pp:4)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end teams distribute parallel do simd
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute simd default(shared) safelen(8) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) collapse (2) &
    !$omp & lastprivate (s) num_teams (n + 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp end target
    !$omp target device (n + 1) if (n .ne. 6)map (from: n) map (alloc: a(2:o))
    !$omp teams distribute simd default(shared) aligned (pp:4) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & thread_limit (n * 2) dist_schedule (static, 4) lastprivate (s)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end teams distribute simd
    !$omp end target
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction ( + : r )
    !$omp distribute collapse (2) firstprivate (q) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute firstprivate (q) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp end distribute
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do if (n .ne. 6) default(shared) &
    !$omp & ordered schedule (static, 8) private (p) firstprivate (q) &
    !$omp & shared(n)reduction(+:r)dist_schedule(static,4)collapse(2)&
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
	  !$omp ordered
	    p = q
	  !$omp end ordered
	  s = i * 10 + j
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do if(n.ne.6)default(shared)&
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) ordered schedule (static, 8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
        !$omp ordered
          p = q
        !$omp end ordered
	s = i * 10
      end do
    !$omp end distribute parallel do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do simd if(n.ne.6)default(shared)&
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & dist_schedule (static, 4) collapse (2) safelen(8) &
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s) &
    !$omp & schedule (static, 8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do simd if (n .ne. 6)default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) schedule (static, 8) &
    !$omp & safelen(16) linear(i:1) aligned (pp:4)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end distribute parallel do simd
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute simd safelen(8) lastprivate(s) &
    !$omp & private (p) firstprivate (q) reduction (+: r) &
    !$omp & dist_schedule (static, 4) collapse (2)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute simd aligned (pp:4) &
    !$omp & private (p) firstprivate (q) reduction (+: r) &
    !$omp & dist_schedule (static, 4) lastprivate (s)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end distribute simd
    !$omp end target teams
  !$omp end target data
  end subroutine
  subroutine bar (n, o, p, r, pp)
    integer :: n, o, p, q, r, s, i, j
    integer :: a (2:o)
    integer, pointer :: pp
    common /blk/ i, j, q
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction ( + : r )
    !$omp distribute collapse (2) firstprivate (q) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute firstprivate (q) dist_schedule (static, 4)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
      end do
    !$omp end distribute
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do if (n .ne. 6) default(shared) &
    !$omp & ordered schedule (static, 8) private (p) firstprivate (q) &
    !$omp & shared(n)reduction(+:r)dist_schedule(static,4)collapse(2)&
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
	  !$omp ordered
	    p = q
	  !$omp end ordered
	  s = i * 10 + j
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do if(n.ne.6)default(shared)&
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) ordered schedule (static, 8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          call dosomething (a, n, p + q)
        end do
        !$omp ordered
          p = q
        !$omp end ordered
	s = i * 10
      end do
    !$omp end distribute parallel do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do simd if(n.ne.6)default(shared)&
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & dist_schedule (static, 4) collapse (2) safelen(8) &
    !$omp & num_threads (n + 4) proc_bind (spread) lastprivate (s) &
    !$omp & schedule (static, 8)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute parallel do simd if (n .ne. 6)default(shared) &
    !$omp & private (p) firstprivate (q) shared (n) reduction (+: r) &
    !$omp & dist_schedule (static, 4) num_threads (n + 4) &
    !$omp & proc_bind (master) lastprivate (s) schedule (static, 8) &
    !$omp & safelen(16) linear(i:1) aligned (pp:4)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end distribute parallel do simd
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute simd safelen(8) lastprivate(s) &
    !$omp & private (p) firstprivate (q) reduction (+: r) &
    !$omp & dist_schedule (static, 4) collapse (2)
      do i = 1, 10
        do j = 1, 10
          r = r + 1
          p = q
          a(2+i*10+j) = p + q
	  s = i * 10 + j
        end do
      end do
    !$omp end target teams
    !$omp target teams device (n + 1) if (n .ne. 6)map (from: n) &
    !$omp & map (alloc: a(2:o)) num_teams (n + 4) thread_limit (n * 2) &
    !$omp & default(shared) shared(n) private (p) reduction(+:r)
    !$omp distribute simd aligned (pp:4) &
    !$omp & private (p) firstprivate (q) reduction (+: r) &
    !$omp & dist_schedule (static, 4) lastprivate (s)
      do i = 1, 10
        r = r + 1
        p = q
        a(1+i) = p + q
	s = i * 10
      end do
    !$omp end distribute simd
    !$omp end target teams
  end subroutine
end module
