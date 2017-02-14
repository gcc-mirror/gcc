! { dg-do compile { target { ! *-*-* } } }
!
program bug
  use H5GLOBAL
  implicit none
  integer :: i
  i=H5P_DEFAULT_F
end program bug
