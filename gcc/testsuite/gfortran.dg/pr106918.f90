! { dg-do run }
!
! Contributed by Lionel Guez  <guez@lmd.ens.fr>
!
  character(len = :), allocatable:: attr_name(:)
  character(6) :: buffer
  type coord_def
     character(len = :), allocatable:: attr_name(:)
  end type coord_def
  type(coord_def) coordinates
  attr_name = ["units"]
  write (buffer, *) attr_name
  if (buffer .ne. " units") stop 1
  coordinates = coord_def(attr_name)
  write (buffer, *) coordinates%attr_name
  if (buffer .ne. " units") stop 2
  deallocate (attr_name)
  deallocate (coordinates%attr_name)
end
