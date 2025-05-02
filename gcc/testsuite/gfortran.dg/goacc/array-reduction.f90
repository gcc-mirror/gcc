! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

program test
  implicit none
  integer a(10), i

  a(:) = 0

  ! Array reductions.

  !$acc parallel reduction (+:a)
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction (+:a)
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc kernels
  !$acc loop reduction (+:a)
  do i = 1, 10
     a = a + 1
  end do
  !$acc end kernels

  !$acc serial reduction (+:a)
  do i = 1, 10
     a = a + 1
  end do
  !$acc end serial

  !$acc serial
  !$acc loop reduction (+:a)
  do i = 1, 10
     a = a + 1
  end do
  !$acc end serial


  ! Subarray reductions.

  !$acc parallel reduction (+:a(1:5))
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction (+:a(1:5))
  do i = 1, 10
     a = a + 1
  end do
  !$acc end parallel

  !$acc kernels
  !$acc loop reduction (+:a(1:5))
  do i = 1, 10
     a = a + 1
  end do
  !$acc end kernels

  !$acc serial reduction (+:a(1:5))
  do i = 1, 10
     a = a + 1
  end do
  !$acc end serial

  !$acc serial
  !$acc loop reduction (+:a(1:5))
  do i = 1, 10
     a = a + 1
  end do
  !$acc end serial


  ! Reductions on array elements.

  !$acc parallel reduction (+:a(1))
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop reduction (+:a(1))
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end parallel

  !$acc kernels
  !$acc loop reduction (+:a(1))
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end kernels

  !$acc serial reduction (+:a(1))
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end serial

  !$acc serial
  !$acc loop reduction (+:a(1))
  do i = 1, 10
     a(1) = a(1) + 1
  end do
  !$acc end serial


  print *, a
end program test

! { dg-final { scan-tree-dump-times "(?n)#pragma acc loop private\\(i\\) reduction\\(\\+:MEM.*\\\[.*&a.*\\\]\\)" 9 "gimple" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_parallel reduction\\(\\+:MEM.*\\\[.*&a.*\\\]\\) map\\(tofrom:a \\\[len: \[0-9\]+\\\]\\)" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_serial reduction\\(\\+:MEM.*\\\[.*&a.*\\\]\\) map\\(tofrom:a \\\[len: \[0-9\]+\\\]\\)" 3 "gimple" } }
