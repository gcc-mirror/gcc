! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single -fdump-tree-original" }
! { dg-additional-options "-latomic" { target libatomic_available } }
!
! Allocate/deallocate with libcaf.
!

program test_caf_alloc

  type t
    integer, allocatable :: i
    real, allocatable :: r(:)
  end type t

  type(t), allocatable :: xx[:]

  allocate (xx[*])

  if (allocated(xx%i)) STOP 1
  if (allocated(xx[1]%i)) STOP 2
  if (allocated(xx[1]%r)) STOP 3
  allocate(xx%i)
  if (.not. allocated(xx[1]%i)) STOP 4
  if (allocated(xx[1]%r)) STOP 5
  
  allocate(xx%r(5))
  if (.not. allocated(xx[1]%i)) STOP 6
  if (.not. allocated(xx[1]%r)) STOP 7
  
  deallocate(xx%i)
  if (allocated(xx[1]%i)) STOP 8
  if (.not. allocated(xx[1]%r)) STOP 9

  deallocate(xx%r)
  if (allocated(xx[1]%i)) STOP 10
  if (allocated(xx[1]%r)) STOP 11

  deallocate(xx)
end

! { dg-final { scan-tree-dump-times "_gfortran_caf_is_present_on_remote" 10 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_register \\(\[0-9\]+, 1, &xx\\.token, \\(void \\*\\) &xx, 0B, 0B, 0\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_register \\(\[0-9\]+, 7" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_register \\(\[0-9\]+, 8" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister \\(&xx\\.token, 0, 0B, 0B, 0\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister \\(&\\(\\(struct t \\* restrict\\) xx\\.data\\)->r\\.token, 1, 0B, 0B, 0\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister \\(&\\(\\(struct t \\* restrict\\) xx\\.data\\)->_caf_i, 1, 0B, 0B, 0\\)" 1 "original" } }
