! { dg-do compile }
! Tests the fix for PR25532, which was a regression introduced by
! the fix for PR20244.
!
! Contributed by Erik Edelmann  <eedelman@gcc.gnu.org>
module ModelParams
        implicit none

        type ReionizationParams
             real   :: fraction
        end type ReionizationParams

        type CAMBparams
             type(ReionizationParams) :: Reion
         end type CAMBparams

        type(CAMBparams) CP
end module ModelParams


module ThermoData
    use ModelParams
    implicit none

contains

    subroutine inithermo()
        use ModelParams
        if (0 < CP%Reion%fraction) then
        end if
    end subroutine inithermo

! The bug expressed itself in this subroutine because the component type
! information was not being copied from the parent namespace.
    subroutine SetTimeSteps
        if (0 < CP%Reion%fraction) then
        end if
    end subroutine SetTimeSteps

end module ThermoData