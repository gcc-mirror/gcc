! { dg-do compile }
! Tests the fix for PR28788, a regression in which an ICE was caused
! by the failure of derived type association for the arguments of
! InitRECFAST because the formal namespace derived types references
! were not being reassociated to the module.
!
! Contributed by Martin Reinecke  <martin@mpa-garching.mpg.de>  
! 
module Precision
  integer, parameter :: dl = KIND(1.d0)
end module Precision

module ModelParams
  use precision
  type CAMBparams
    real(dl)::omegab,h0,tcmb,yhe
  end type
  type (CAMBparams) :: CP
contains
  subroutine CAMBParams_Set(P)
    type(CAMBparams), intent(in) :: P
  end subroutine CAMBParams_Set
end module ModelParams

module TimeSteps
  use precision
  use ModelParams
end module TimeSteps

module ThermoData
  use TimeSteps
contains
  subroutine inithermo(taumin,taumax)
    use precision
    use ModelParams  ! Would ICE here
    real(dl) taumin,taumax
    call InitRECFAST(CP%omegab,CP%h0,CP%tcmb,CP%yhe)
  end subroutine inithermo
end module ThermoData
! { dg-final { cleanup-modules "PRECISION ModelParams TimeSteps ThermoData" } }
