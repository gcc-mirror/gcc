! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! { dg-options "-fdump-tree-original" }
! { dg-output "1234" }
! Ordering of images through a spin lock using atomic_cas.

program main
  use iso_fortran_env, only : atomic_int_kind
  implicit none
  integer (atomic_int_kind) :: atom[*]
  integer (atomic_int_kind) :: old, compare, new
  integer :: me, n
  atom = 0
  me = this_image ()
  n = num_images()
  sync all
  compare = me - 1
  new = -1
  wait: do
     call atomic_cas (atom[1], old, compare, new)
     if (old == compare) exit wait
  end do wait
  sync memory
  write (*,'(I0)',advance="no") this_image()
  call atomic_define (atom[1], this_image())
  sync memory
end program main
! { dg-final { scan-tree-dump-times "__atomic_compare_exchange" 1 "original" } }
