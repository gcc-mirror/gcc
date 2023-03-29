! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/108512

! The problem was that the context wasn't reset for the 'LOOP'
! such that the clauses of the loops weren't seen when adding
! PRIVATE clauses.
!
! In the following, only the loop variable of the non-OpenMP loop
! in 'subroutine four' should get a front-end addded PRIVATE clause

implicit none
integer :: x, a(10), b(10), n
    n = 10
    a = -42
    b = [(2*x, x=1,10)]

! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:a\\) map\\(tofrom:b\\) map\\(tofrom:x\\)\[\r\n\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel\[\r\n\]" 2 "original" } }
!  ^- shows up twice; checked only here.
! { dg-final { scan-tree-dump-times "#pragma omp loop lastprivate\\(x\\)\[\r\n\]" 1 "original" } }

    !$omp target parallel map(tofrom: a, b, x)
    !$omp loop lastprivate(x)
    DO x = 1, n
      a(x) = a(x) + b(x)
    END DO
    !$omp end loop
    !$omp end target parallel
    if (x /= 11) error stop
    if (any (a /= [(2*x - 42, x=1,10)])) error stop
    call two()
    call three()
    call four()
end

subroutine two
  implicit none
  integer :: ii, mm, arr(10)
  mm = 10
  arr = 0

! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:arr\\) map\\(tofrom:ii\\)\[\r\n\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel shared\\(ii\\)\[\r\n\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp loop lastprivate\\(ii\\)\[\r\n\]" 1 "original" } }

  !$omp target parallel loop map(tofrom: arr) lastprivate(ii)
    DO ii = 1, mm
      arr(ii) = arr(ii) + ii
    END DO
end

subroutine three
  implicit none
  integer :: kk, zz, var(10)
  zz = 10
  var = 0

! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:var\\)\[\r\n\]" 1 "original" } }
! "#pragma omp parallel\[\r\n\]" - shows up twice, dump checked above
! { dg-final { scan-tree-dump-times "#pragma omp loop\[\r\n\]" 1 "original" } }

  !$omp target parallel loop map(tofrom: var)
    DO kk = 1, zz
      var(kk) = var(kk) + kk
    END DO
end

subroutine four
  implicit none
  integer :: jj, qq, dist(10)
  qq = 10
  dist = 0

! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:dist\\)\[\r\n\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel private\\(jj\\)\[\r\n\]" 1 "original" } }

  !$omp target parallel map(tofrom: dist)
    ! *no* '!$omp do/loop/simd'
    DO jj = 1, qq
      dist(qq) = dist(qq) + qq
    END DO
  !$omp end target parallel
end
