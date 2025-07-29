! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/93834 - ICE in trans_caf_is_present

program p
  type t
    integer, allocatable :: x[:,:,:]
  end type t
  integer, allocatable :: a[:]
  type(t) :: c
  if (allocated (a)) stop 1
  if (allocated (c%x)) stop 2

  ! The coindexed scalar (!) variable is regarded as allocatable but
  ! we can check the value on any image of the team as they are
  ! established collectively.  As tested by the dump, we do it on
  ! this_image ().
  !
  ! For this reason, -fcoarray=single and -fcoarray=lib give the
  ! same result
  if (allocated (a[1])) stop 3
  if (allocated (c%x[1,2,3])) stop 4

  ! Allocate collectively
  allocate(a[*])
  allocate(c%x[4,10,*])

  if (.not. allocated (a)) stop 5
  if (.not. allocated (c%x)) stop 6
  if (.not. allocated (a[1])) stop 7
  if (.not. allocated (c%x[1,2,3])) stop 8

  ! Deallocate collectively
  deallocate(a)
  deallocate(c%x)

  if (allocated (a)) stop 9
  if (allocated (c%x)) stop 10
  if (allocated (a[1])) stop 11
  if (allocated (c%x[1,2,3])) stop 12
end

! Expected: always local access and never a call to _gfortran_caf_get
! { dg-final { scan-tree-dump-not "caf_get " "original" } }
! { dg-final { scan-tree-dump-not "caf_get_by_" "original" } }
