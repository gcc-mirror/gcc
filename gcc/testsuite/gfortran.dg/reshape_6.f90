! { dg-do compile }
! PR fortran/58989
!
program test

  real(8), dimension(4,4) :: fluxes
  real(8), dimension(2,2,2,2) :: f
  integer, dimension(3) :: dmmy 
  integer, parameter :: indx(4)=(/2,2,2,2/)

  fluxes = 1

  dmmy = (/2,2,2/)

  f = reshape(fluxes,(/dmmy,2/))  ! Caused an ICE
  f = reshape(fluxes,(/2,2,2,2/)) ! Works as expected
  f = reshape(fluxes,indx)        ! Works as expected

end program test
