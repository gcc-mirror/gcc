! { dg-do run }
! Verify that the changes made to character dummy arguments for bind(c) 
! procedures doesn't break non-bind(c) routines.
! PR fortran/32732
subroutine bar(a)
  use, intrinsic :: iso_c_binding, only: c_char
  character(c_char), value :: a
  if(a /= c_char_'a') STOP 1
end subroutine bar

subroutine bar2(a)
  use, intrinsic :: iso_c_binding, only: c_char
  character(c_char) :: a
  if(a /= c_char_'a') STOP 2
end subroutine bar2

use iso_c_binding
implicit none
interface
  subroutine bar(a)
    import
    character(c_char),value :: a
  end subroutine bar
  subroutine bar2(a)
    import
    character(c_char) :: a
  end subroutine bar2
end interface
 character(c_char) :: z
 z = 'a'
 call bar(z)
 call bar2(z)
end
