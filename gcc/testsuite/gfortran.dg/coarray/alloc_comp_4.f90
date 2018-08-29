! { dg-do run }

! Contributed by Damian Rouson

program main

  implicit none

  type mytype
    integer, allocatable :: indices(:)
  end type

  type(mytype), save :: object[*]
  integer :: me

  me=this_image()
  allocate(object%indices(me))
  object%indices = 42

  if ( any( object[me]%indices(:) /= 42 ) ) STOP 1
end program
