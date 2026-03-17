! { dg-do run }

! Contributed by Damian Rouson

program main

  implicit none

  type mytype
    integer, allocatable :: indices(:)
  end type

  type(mytype), save :: object[*]
  integer :: me, other

  me=this_image()
  other = me + 1
  if (other .GT. num_images()) other = 1
  if (me == num_images()) then
     allocate(object%indices(me/2))
  else
    allocate(object%indices(me))
  end if
  object%indices = 42 * me

  sync all
  if ( any( object[other]%indices(:) /= 42 * other ) ) STOP 1
  sync all
end program
