! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/91300 - runtime error message with allocate and errmsg=
! Contributed by zed.three

program bigarray_prog
  use, intrinsic :: iso_c_binding, only: C_INTPTR_T
  implicit none
  real(4), dimension(:), allocatable :: array, bigarray
  integer                 :: stat1, stat2
  character(len=100)      :: errmsg1, errmsg2
  character(*), parameter :: no_error = "no error"
  integer(8), parameter :: n1 = huge (1_4) / 3          ! request more than 2GB
  integer(8), parameter :: n2 = huge (1_C_INTPTR_T) / 4 ! "safe" for 64bit
  integer(8), parameter :: bignumber = max (n1, n2)

  stat1   = -1
  stat2   = -1
  errmsg1 = no_error
  errmsg2 = no_error
  allocate (array(1), stat=stat1, errmsg=errmsg1)
  if (stat1   /= 0       ) stop 1
  if (errmsg1 /= no_error) stop 1

  ! Obtain stat, errmsg for attempt to allocate an allocated object
  allocate (array(1), stat=stat1, errmsg=errmsg1)
  if (stat1   == 0       ) stop 2
  if (errmsg1 == no_error) stop 2

  ! Try to allocate very large object
  allocate (bigarray(bignumber), stat=stat2, errmsg=errmsg2)
  if (stat2 /= 0) then
     print *, "stat1 =", stat1
     print *, "errmsg: ", trim (errmsg1)
     print *, "stat2 =", stat2
     print *, "errmsg: ", trim (errmsg2)
     ! Ensure different results for stat, errmsg variables (all compilers)
     if (stat2   == stat1                           ) stop 3
     if (errmsg2 == no_error .or. errmsg2 == errmsg1) stop 4

     ! Finally verify gfortran-specific error messages
     if (errmsg1 /= "Attempt to allocate an allocated object") stop 5
     if (errmsg2 /= "Insufficient virtual memory"            ) stop 6
  end if

end program bigarray_prog

! { dg-final { scan-tree-dump-times "Attempt to allocate an allocated object" 4 "original" } }
! { dg-final { scan-tree-dump-times "Insufficient virtual memory" 4 "original" } }
