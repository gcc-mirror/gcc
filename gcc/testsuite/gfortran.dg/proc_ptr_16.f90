! { dg-do compile }
!
! PR 39946: PROCEDURE statements: interface with RESULT variable
!
! Original test case by Juergen Reuter <reuter@physik.uni-freiburg.de>
! Modified by Janus Weil <janus@gcc.gnu.org>

  procedure(prc_is_allowed), pointer :: fptr

  interface
     function prc_is_allowed (flv, hel, col) result (is_allowed)
       logical :: is_allowed
       integer, intent(in) :: flv, hel, col
     end function prc_is_allowed
  end interface

  fptr => prc_is_allowed

end

