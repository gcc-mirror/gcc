! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single -fdump-tree-original" }

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
  if ( size(object%indices) /= 1 ) call abort()
  ! therefore no array is present here and no array test needed.
  if ( object%indices(1) /= 1 ) call abort()
end program

! { dg-final { scan-tree-dump-times "_gfortran_caf_register \\(D.\[0-9\]+, 1, &\\(\\(struct mytype\\) \\*object\\).indices.token, &\\(\\(struct mytype\\) \\*object\\).indices, 0B, 0B, 0\\);" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister \\(&\\(\\(struct mytype\\) \\*object\\).indices.token, 0B, 0B, 0\\);" 1 "original" } }

