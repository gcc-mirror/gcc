! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/97061

integer function f3 (a1, b1, u)
  implicit none
  integer :: a1, b1, d1
  integer u(0:1023)
  !$omp teams distribute parallel do simd default(none) firstprivate (a1, b1) shared(u) lastprivate(d1) 
  do d1 = a1, b1-1
      u(d1) = 5
  end do
end

subroutine foo(n, m, u)
  implicit none
  integer :: hh, ii, jj, n, m
  integer u(0:1023)
  !$omp simd private(ii)
  do ii = n, m
    u(ii) = 5
  end do
  !$omp simd linear(jj:1)
  do jj = 2, m+n
    u(jj) = 6
  end do
  !$omp simd
  do hh = 2, m+n
    u(hh) = 6
  end do
end

subroutine bar(n, m, u)
  implicit none
  integer :: kkk, lll, ooo, ppp, n, m
  integer u(:,:)
  !$omp simd lastprivate(kkk) lastprivate(lll) collapse(2)
  do kkk = n, m
    do lll = n, m
      u(kkk, lll) = 5
    end do
  end do
  !$omp simd private(kkk) private(lll) collapse(2)
  do ooo = n, m
    do ppp = n, m
      u(ooo, ppp) = 5
    end do
  end do
end


! { dg-final { scan-tree-dump-times "#pragma omp teams firstprivate\\(a1\\) firstprivate\\(b1\\) shared\\(u\\) shared\\(d1\\) default\\(none\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp distribute lastprivate\\(d1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel firstprivate\\(a1\\) firstprivate\\(b1\\) lastprivate\\(d1\\) shared\\(u\\) default\\(none\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd lastprivate\\(d1\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp simd private\\(ii\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(jj:1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(hh:1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd lastprivate\\(kkk\\) lastprivate\\(lll\\) collapse\\(2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd private\\(kkk\\) private\\(lll\\) collapse\\(2\\)" 1 "original" } }
