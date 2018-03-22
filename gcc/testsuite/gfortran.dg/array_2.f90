! { dg-do run }
! PR tree-optimization/30092
! This caused once an ICE due to internal tree changes
program test
  implicit none
  integer, parameter :: N = 30
  real, dimension(N) :: rho, pre, cs
  real               :: gamma
  gamma = 2.1314
  rho = 5.0
  pre = 3.0
  call EOS(N, rho, pre, cs, gamma)
  if (abs(CS(1) - sqrt(gamma*pre(1)/rho(1))) > epsilon(cs)) &
     STOP 1
contains
      SUBROUTINE EOS(NODES, DENS, PRES, CS, CGAMMA)
      IMPLICIT NONE
      INTEGER NODES
      REAL CGAMMA
      REAL, DIMENSION(NODES) :: DENS, PRES, CS
      REAL, PARAMETER :: RGAS = 8.314
      CS(:NODES) = SQRT(CGAMMA*PRES(:NODES)/DENS(:NODES))
      END SUBROUTINE EOS
end program test
