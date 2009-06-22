! { dg-do compile }
!
! PR 37254: Reject valid PROCEDURE statement with implicit interface
!
! Original test case by Dominique d'Humieres <dominiq@lps.ens.fr>
! Modified by Janus Weil <janus@gcc.gnu.org>

  real function proc3( arg1 )
     integer :: arg1
     proc3 = arg1+7
  end function proc3

program myProg
  PROCEDURE () :: proc3
  call proc4( proc3 )

contains

  subroutine proc4( arg1 )
     PROCEDURE(real) :: arg1
     print*, 'the func: ', arg1(0)
  end subroutine proc4

end program myProg

