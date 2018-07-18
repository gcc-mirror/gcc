! Test that IOLENGTH works for derived types containing arrays
module iolength_2_mod
  integer, parameter ::  &
       ! 32 bit, i.e. 4 byte integer (every gcc architecture should have this?)
       int32 = selected_int_kind(9), &
       ! IEEE double precision, i.e. 8 bytes
       dp = selected_real_kind(15, 307)
  type foo
     ! This type should take up 5*4+4+8=32 bytes
     integer(int32) :: a(5), b
     real(dp) :: c
  end type foo
end module iolength_2_mod

program iolength_2
  use iolength_2_mod
  implicit none
  integer :: iol
  type(foo) :: d
  inquire (iolength = iol) d
  if ( 32 /= iol) then
     STOP 1
  end if
end program iolength_2
