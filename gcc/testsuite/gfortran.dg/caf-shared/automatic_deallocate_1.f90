! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! { dg-options "-fdump-tree-original" }

program main
  integer :: n
  n = 4096
  do i=1,3
     block
       integer, allocatable :: a[:]
       if (allocated(a)) stop 1
       allocate (a[*])
       a = 42
       n = n * 2
     end block
  end do
end program main
! { dg-final { scan-tree-dump-times "_gfortran_cas_coarray_alloc_chk" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_cas_coarray_free" 1 "original" } }
