! Test that IOLENGTH works for io list containing more than one entry
program iolength_3
  implicit none
  integer, parameter ::  &
       ! 32 bit, i.e. 4 byte integer (every gcc architecture should have this?)
       int32 = selected_int_kind(9), &
       ! IEEE double precision, i.e. 8 bytes
       dp = selected_real_kind(15, 307)
  integer(int32) :: a, b, iol
  real(dp) :: c
  inquire (iolength = iol) a, b, c
  if ( 16 /= iol) then
     call abort
  end if
end program iolength_3
