! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! { dg-options "-fdump-tree-original" }
program atomic
  use iso_fortran_env
  implicit none
  integer(atomic_int_kind) :: atom[*]
  integer :: to_set[*]
  integer :: val, i
  if (this_image() == 1) then
     call atomic_define (atom[1], 0)
  end if
  sync all
  ! Spin loop on image 2 setting atom[1]
  if (this_image () == 2) then
     do i=1,num_images()
        to_set[i] = 42
     end do
     call atomic_define (atom[1], 1)
     sync memory
  else
     wait: do
        call atomic_ref (val, atom[1])
        if (val == 1) exit wait
     end do wait
     sync memory
     if (to_set /= 42) stop 42
  end if
end program atomic
! { dg-final { scan-tree-dump-times "__atomic_load_4" 1 "original" } }
! { dg-final { scan-tree-dump-times "__atomic_store_4" 2 "original" } }
! { dg-final { scan-tree-dump-times "__atomic_thread_fence" 3 "original" } }
