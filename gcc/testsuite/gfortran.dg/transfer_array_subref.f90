! { dg-do run }
! { dg-additional-options "-O2 -fdump-tree-optimized" }
!
! PR fortran/102891 - passing of inquiry ref of complex array to TRANSFER

program main
  implicit none
  integer, parameter :: dp = 8

  type complex_wrap1
     complex(dp) :: z(2)
  end type complex_wrap1

  type complex_wrap2
     complex(dp), dimension(:), allocatable :: z
  end type complex_wrap2

  type(complex_wrap1) :: x = complex_wrap1([ (1, 2), (3, 4) ])
  type(complex_wrap2) :: w

  w%z = x%z

  ! The following statements should get optimized away...
  if (size (transfer ( x%z%re ,[1.0_dp])) /= 2) error stop 1
  if (size (transfer ((x%z%re),[1.0_dp])) /= 2) error stop 2
  if (size (transfer ([x%z%re],[1.0_dp])) /= 2) error stop 3
  if (size (transfer ( x%z%im ,[1.0_dp])) /= 2) error stop 4
  if (size (transfer ((x%z%im),[1.0_dp])) /= 2) error stop 5
  if (size (transfer ([x%z%im],[1.0_dp])) /= 2) error stop 6

  ! ... while the following may not:
  if (any  (transfer ( x%z%re ,[1.0_dp])  /= x%z%re)) stop 7
  if (any  (transfer ( x%z%im ,[1.0_dp])  /= x%z%im)) stop 8

  if (size (transfer ( w%z%re ,[1.0_dp])) /= 2) stop 11
  if (size (transfer ((w%z%re),[1.0_dp])) /= 2) stop 12
  if (size (transfer ([w%z%re],[1.0_dp])) /= 2) stop 13
  if (size (transfer ( w%z%im ,[1.0_dp])) /= 2) stop 14
  if (size (transfer ((w%z%im),[1.0_dp])) /= 2) stop 15
  if (size (transfer ([w%z%im],[1.0_dp])) /= 2) stop 16

  if (any  (transfer ( w%z%re ,[1.0_dp])  /= x%z%re)) stop 17
  if (any  (transfer ( w%z%im ,[1.0_dp])  /= x%z%im)) stop 18

  deallocate (w%z)
end program main

! { dg-final { scan-tree-dump-not "_gfortran_error_stop_numeric" "optimized" } }
