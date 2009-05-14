! { dg-do compile }
! Test fix for PR16943 in which the double typing of
! N caused an error.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  program bug8 
    implicit none 
    stop " OK. " 

  contains 

    integer function bugf(M) result (N) 
      integer, intent (in) :: M 
      integer :: N ! { dg-error "already has basic type of INTEGER" }
      N = M 
      return 
    end function bugf
  end program bug8
