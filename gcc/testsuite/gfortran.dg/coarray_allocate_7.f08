! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single -fdump-tree-original" }
! { dg-additional-options "-latomic" { target libatomic_available } }

! Contributed by Damian Rouson
! Checking whether (de-)registering of coarrays works.

program main

  implicit none

  type mytype
    integer, allocatable :: indices(:)
  end type

  type(mytype), save :: object[*]
  integer :: i,me

  me=this_image() ! me is always 1 here
  object%indices=[(i,i=1,me)]
  if ( size(object%indices) /= 1 ) STOP 1
  ! therefore no array is present here and no array test needed.
  if ( object%indices(1) /= 1 ) STOP 2
end program

! { dg-final { scan-tree-dump-times "_gfortran_caf_register \\(0, 7, \\(void \\*\\) &mytype\\.\[0-9\]+\\.indices\\.token, &mytype\\.\[0-9\]+\\.indices, 0B, 0B, 0\\);" 1 "original" } }

