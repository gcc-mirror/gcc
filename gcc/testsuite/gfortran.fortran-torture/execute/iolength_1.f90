! Test that IOLENGTH works for dynamic arrays
program iolength_1
  implicit none
  ! 32 bit, i.e. 4 byte integer (every gcc architecture should have this?)
  integer, parameter :: int32 = selected_int_kind(9)
  integer(int32), allocatable :: a(:)
  integer :: iol, alength
  real :: r
  call random_number(r)
  alength = nint(r*20)
  allocate(a(alength))
  inquire (iolength = iol) a
  if ( 4*alength /= iol) then
     call abort
  end if
end program iolength_1
