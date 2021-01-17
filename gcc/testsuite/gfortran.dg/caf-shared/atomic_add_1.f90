! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! { dg-options "-fdump-tree-original" }
! Hand-crafted spin lock to make sure that atomic_add works.

program main
  use iso_fortran_env, only : atomic_int_kind
  implicit none
  integer (atomic_int_kind) :: x[*]
  integer (atomic_int_kind) :: val
  integer :: n, me
  character (len=20) :: line
  integer :: i
  n = num_images()
  me = this_image()
  x = 0
  sync all
  ! Burn some CPU time to provoke race conditions.
  do i=1,1000
     write (unit=line,fmt='(F12.5)') sin(i*1.d0)
  end do
  call atomic_add (x[1], 1)
  wait: do
     call atomic_ref (val, x[1])
     if (val == n) exit wait
  end do wait
  sync memory
end program main
! { dg-final { scan-tree-dump-times "__atomic_fetch_add_4" 1 "original" } }
