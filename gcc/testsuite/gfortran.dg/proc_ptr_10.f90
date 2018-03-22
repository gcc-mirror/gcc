! { dg-do run }
!
! PR fortran/37253
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>

module myMod

  CONTAINS

  real function proc3( arg1 )
     integer :: arg1
     proc3 = arg1+7
  end function proc3

  subroutine proc4( arg1 )
     procedure(real), pointer :: arg1
     if (arg1(0)/=7) STOP 1
  end subroutine proc4

end module myMod

program myProg
  use myMod
  PROCEDURE (real), POINTER :: p => NULL()
  p => proc3
  call proc4( p )
end program myProg
 
