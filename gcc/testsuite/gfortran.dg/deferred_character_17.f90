!{ dg-do run }

! Check fix for PR fortran/71623

program allocatemvce
  implicit none
  character(len=:), allocatable :: string
  integer, dimension(4), target :: array = [1,2,3,4]
  integer, dimension(:), pointer :: array_ptr
  array_ptr => array
  ! The allocate used to segfault
  allocate(character(len=size(array_ptr))::string)
end program allocatemvce
