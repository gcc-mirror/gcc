! { dg-do compile }
! Test fix for PR16943 in which the double typing of
! N caused an error.  This is a common extension to the
! F95 standard, so the error is only thrown for -std=f95
! or -pedantic.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  program bug8 
    implicit none 
    stop " OK. " 

  contains 

    integer function bugf(M) result (N) 
      integer, intent (in) :: M 
      integer :: N ! { dg-warning "already has basic type of INTEGER" }
      N = M 
      return 
    end function bugf
  end program bug8
