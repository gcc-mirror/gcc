! { dg-additional-options "-fdump-tree-gimple" }

! Test that reduction on compute construct implies a copy of the reduction variable

subroutine test
  implicit none
  integer i
  integer a(100), s1, p1
  integer r1
  real b(100), s2
  logical c(100), r2
  double precision d(100), s3
  complex e(100), s4
  p1 = 1

  !$acc parallel reduction(+:s1, s2, s3, s4) reduction(*:p1)
  do i = 1, 100
    s1 = s1 + a(i)
    p1 = p1 * a(i)
    s2 = s2 +  b(i)
    s3 = s3 +  d(i)
    s4 = s4 +  e(i)
  end do
  !$acc end parallel

  !$acc parallel reduction (max:r1)
  do i = 1,10
    if (r1 <= a(i)) then
      r1 = a(i)
    end if
  end do
  !$acc end parallel

  !$acc parallel reduction (min:r1)
  do i = 1,10
    if (r1 >= a(i)) then
      r1 = a(i)
    end if
  end do
  !$acc end parallel

  !$acc parallel reduction (iand:r1)
  do i = 1,10
    r1 = iand (r1, a(i))
  end do
  !$acc end parallel

  !$acc parallel reduction (ior:r1)
  do i = 1,10
    r1 = ior (r1, a(i))
  end do
  !$acc end parallel

  !$acc parallel reduction (ieor:r1)
  do i = 1,10
    r1 = ieor (r1, a(i))
  end do
  !$acc end parallel

  !$acc parallel reduction (.and.:r2)
  do i = 1,10
    r2 = r2 .and. c(i)
  end do
  !$acc end parallel

  !$acc parallel reduction (.or.:r2)
  do i = 1,10
    r2 = r2 .or. c(i)
  end do
  !$acc end parallel

  !$acc parallel reduction (.eqv.:r2)
  do i = 1,10
    r2 = r2 .eqv. c(i)
  end do
  !$acc end parallel

  !$acc parallel reduction (.neqv.:r2)
  do i = 1,10
    r2 = r2 .neqv. c(i)
  end do
  !$acc end parallel

  !$acc serial reduction(+:s1, s2, s3, s4) reduction(*:p1)
  do i = 1, 100
    s1 = s1 + a(i)
    p1 = p1 * a(i)
    s2 = s2 +  b(i)
    s3 = s3 +  d(i)
    s4 = s4 +  e(i)
  end do
  !$acc end serial

  !$acc serial reduction (max:r1)
  do i = 1,10
    if (r1 <= a(i)) then
      r1 = a(i)
    end if
  end do
  !$acc end serial

  !$acc serial reduction (min:r1)
  do i = 1,10
    if (r1 >= a(i)) then
      r1 = a(i)
    end if
  end do
  !$acc end serial

  !$acc serial reduction (iand:r1)
  do i = 1,10
    r1 = iand (r1, a(i))
  end do
  !$acc end serial

  !$acc serial reduction (ior:r1)
  do i = 1,10
    r1 = ior (r1, a(i))
  end do
  !$acc end serial

  !$acc serial reduction (ieor:r1)
  do i = 1,10
    r1 = ieor (r1, a(i))
  end do
  !$acc end serial

  !$acc serial reduction (.and.:r2)
  do i = 1,10
    r2 = r2 .and. c(i)
  end do
  !$acc end serial

  !$acc serial reduction (.or.:r2)
  do i = 1,10
    r2 = r2 .or. c(i)
  end do
  !$acc end serial

  !$acc serial reduction (.eqv.:r2)
  do i = 1,10
    r2 = r2 .eqv. c(i)
  end do
  !$acc end serial

  !$acc serial reduction (.neqv.:r2)
  do i = 1,10
    r2 = r2 .neqv. c(i)
  end do
  !$acc end serial

end subroutine test

! { dg-final { scan-tree-dump-times "map\\(tofrom:s1 \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:r1 \\\[len: \[0-9\]+\\\]\\)" 10 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:r2 \\\[len: \[0-9\]+\\\]\\)" 8 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:s2 \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:s3 \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:s4 \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "map\\(tofrom:p1 \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } }
