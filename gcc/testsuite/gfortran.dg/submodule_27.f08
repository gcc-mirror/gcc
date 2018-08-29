! { dg-do run }
!
! Tests the fix for PR71838 in which the PROCEDURE dummy argument caused
! an ICE in the submodule. This an executable version of the reduced test
! in comment #11.
!
! Contributed by Anton Shterenlikht  <mexas@bristol.ac.uk>
! Test reduced by Dominique d'Humieres <dominiq@lps.ens.fr>
!
subroutine hello (message)
  character (7), intent(inout) :: message
  message = "hello  "
end

module cgca_m3clvg
  interface
    subroutine cgca_clvgs_abstract(message)
      character (7), intent(inout) :: message
    end subroutine cgca_clvgs_abstract
  end interface

  interface
    module subroutine cgca_clvgp(sub)
      procedure( cgca_clvgs_abstract ) :: sub
    end subroutine cgca_clvgp
  end interface

  character (7) :: greeting
end module cgca_m3clvg

submodule ( cgca_m3clvg ) m3clvg_sm3
  implicit none
contains
  module procedure cgca_clvgp
    call sub (greeting)
  end procedure cgca_clvgp
end submodule m3clvg_sm3

  use cgca_m3clvg
  external hello
  greeting = "goodbye"
  call cgca_clvgp (hello)
  if (trim (greeting) .ne. "hello") STOP 1
end
